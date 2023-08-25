(in-package #:sacla-lisp-unit)
(define-test sacla-must-sequence.1 (:tag :sacla)
 (assert-true (eql (length "abc") 3)))
(define-test sacla-must-sequence.2 (:tag :sacla)
 (assert-true
  (let ((str
         (make-array '(3)
                     :element-type 'character
                     :initial-contents "abc"
                     :fill-pointer t)))
    (and (eql (length str) 3)
         (eql (setf (fill-pointer str) 2) 2)
         (eql (length str) 2)))))
(define-test sacla-must-sequence.3 (:tag :sacla)
 (assert-true (zerop (length #*))))
(define-test sacla-must-sequence.4 (:tag :sacla)
 (assert-true (zerop (length ""))))
(define-test sacla-must-sequence.5 (:tag :sacla)
 (assert-true (zerop (length #()))))
(define-test sacla-must-sequence.6 (:tag :sacla)
 (assert-true (zerop (length nil))))
(define-test sacla-must-sequence.7 (:tag :sacla)
 (assert-true (eql (length '(0)) 1)))
(define-test sacla-must-sequence.8 (:tag :sacla)
 (assert-true (eql (length '(0 1)) 2)))
(define-test sacla-must-sequence.9 (:tag :sacla)
 (assert-true (eql (length '(0 1 2)) 3)))
(define-test sacla-must-sequence.10 (:tag :sacla)
 (assert-true (eql (length '(0 1 2 3)) 4)))
(define-test sacla-must-sequence.11 (:tag :sacla)
 (assert-true (eql (length '(0 1 2 3 4)) 5)))
(define-test sacla-must-sequence.12 (:tag :sacla)
 (assert-true (eql (length '(0 1 2 3 4 5)) 6)))
(define-test sacla-must-sequence.13 (:tag :sacla)
 (assert-true (eql (length '(0 1 2 3 4 5 6)) 7)))
(define-test sacla-must-sequence.14 (:tag :sacla)
 (assert-true (eql (length #(0)) 1)))
(define-test sacla-must-sequence.15 (:tag :sacla)
 (assert-true (eql (length #(0 1)) 2)))
(define-test sacla-must-sequence.16 (:tag :sacla)
 (assert-true (eql (length #(0 1 2)) 3)))
(define-test sacla-must-sequence.17 (:tag :sacla)
 (assert-true (eql (length #(0 1 2 3)) 4)))
(define-test sacla-must-sequence.18 (:tag :sacla)
 (assert-true (eql (length #(0 1 2 3 4)) 5)))
(define-test sacla-must-sequence.19 (:tag :sacla)
 (assert-true (eql (length #(0 1 2 3 4 5)) 6)))
(define-test sacla-must-sequence.20 (:tag :sacla)
 (assert-true (eql (length #(0 1 2 3 4 5 6)) 7)))
(define-test sacla-must-sequence.21 (:tag :sacla)
 (assert-true (eql (length (make-array 100)) 100)))
(define-test sacla-must-sequence.22 (:tag :sacla)
 (assert-true (eql (length (make-sequence 'list 20)) 20)))
(define-test sacla-must-sequence.23 (:tag :sacla)
 (assert-true (eql (length (make-sequence 'string 10)) 10)))
(define-test sacla-must-sequence.24 (:tag :sacla)
 (assert-true (eql (length (make-sequence 'bit-vector 3)) 3)))
(define-test sacla-must-sequence.25 (:tag :sacla)
 (assert-true (eql (length (make-sequence 'bit-vector 64)) 64)))
(define-test sacla-must-sequence.26 (:tag :sacla)
 (assert-true (eql (length (make-sequence 'simple-vector 64)) 64)))
(define-test sacla-must-sequence.27 (:tag :sacla)
 (assert-true (string= (copy-seq "love") "love")))
(define-test sacla-must-sequence.28 (:tag :sacla)
 (assert-true (equalp (copy-seq '#(a b c d)) '#(a b c d))))
(define-test sacla-must-sequence.29 (:tag :sacla)
 (assert-true (equalp (copy-seq '#*01010101) '#*01010101)))
(define-test sacla-must-sequence.30 (:tag :sacla)
 (assert-true (equal (copy-seq '(love)) '(love))))
(define-test sacla-must-sequence.31 (:tag :sacla)
 (assert-true (equal (copy-seq '(love hate war peace)) '(love hate war peace))))
(define-test sacla-must-sequence.32 (:tag :sacla)
 (assert-true (null (copy-seq nil))))
(define-test sacla-must-sequence.33 (:tag :sacla)
 (assert-true (string= (copy-seq "") "")))
(define-test sacla-must-sequence.34 (:tag :sacla)
 (assert-true
  (let* ((seq0 "love&peace") (seq (copy-seq seq0)))
    (and (not (eq seq0 seq)) (string= seq0 seq)))))
(define-test sacla-must-sequence.35 (:tag :sacla)
 (assert-true
  (let* ((seq0 (list 'love 'and 'peace)) (seq (copy-seq seq0)))
    (and (not (eq seq0 seq)) (equal seq0 seq)))))
(define-test sacla-must-sequence.36 (:tag :sacla)
 (assert-true
  (let* ((c0 (list 'love)) (c1 (list 'peace)) (seq (copy-seq (list c0 c1))))
    (and (equal seq '((love) (peace))) (eq (car seq) c0) (eq (cadr seq) c1)))))
(define-test sacla-must-sequence.37 (:tag :sacla)
 (assert-true
  (let* ((seq0 '#(t nil t nil)) (seq (copy-seq seq0)))
    (and (not (eq seq0 seq)) (equalp seq seq0)))))
(define-test sacla-must-sequence.38 (:tag :sacla)
 (assert-true (vectorp (copy-seq (vector)))))
(define-test sacla-must-sequence.39 (:tag :sacla)
 (assert-true (simple-bit-vector-p (copy-seq #*))))
(define-test sacla-must-sequence.40 (:tag :sacla)
 (assert-true (simple-vector-p (copy-seq (vector)))))
(define-test sacla-must-sequence.41 (:tag :sacla)
 (assert-true
  (simple-vector-p
   (copy-seq (make-array 10 :fill-pointer 3 :initial-element nil)))))
(define-test sacla-must-sequence.42 (:tag :sacla)
 (assert-true (simple-vector-p (copy-seq (vector 0 1)))))
(define-test sacla-must-sequence.43 (:tag :sacla)
 (assert-true (simple-string-p (copy-seq "xyz"))))
(define-test sacla-must-sequence.44 (:tag :sacla)
 (assert-true
  (simple-string-p
   (copy-seq
    (make-array 3
                :displaced-to "0123456789"
                :displaced-index-offset 3
                :element-type 'base-char)))))
(define-test sacla-must-sequence.45 (:tag :sacla)
 (assert-true
  (simple-string-p
   (copy-seq
    (make-array 20
                :fill-pointer t
                :element-type 'base-char
                :initial-element #\ )))))
(define-test sacla-must-sequence.46 (:tag :sacla)
 (assert-true (simple-bit-vector-p (copy-seq #*0101))))
(define-test sacla-must-sequence.47 (:tag :sacla)
 (assert-true
  (simple-bit-vector-p
   (copy-seq
    (make-array 30 :fill-pointer 3 :element-type 'bit :initial-element 0)))))
(define-test sacla-must-sequence.48 (:tag :sacla)
 (assert-true
  (let* ((vec0 (make-array 10 :fill-pointer 3 :initial-contents "0123456789"))
         (vec (copy-seq vec0)))
    (and (simple-vector-p vec)
         (= (length vec) 3)
         (equalp vec #(#\0 #\1 #\2))))))
(define-test sacla-must-sequence.49 (:tag :sacla)
 (assert-true (char= (elt "0123456789" 6) #\6)))
(define-test sacla-must-sequence.50 (:tag :sacla)
 (assert-true (eq (elt #(a b c d e f g) 0) 'a)))
(define-test sacla-must-sequence.51 (:tag :sacla)
 (assert-true (eq (elt '(a b c d e f g) 4) 'e)))
(define-test sacla-must-sequence.52 (:tag :sacla)
 (assert-true (zerop (elt #*0101010 0))))
(define-test sacla-must-sequence.53 (:tag :sacla)
 (assert-true
  (dotimes (i 10 t)
    (unless (char= (elt "0123456789" i) (digit-char i))
      (return nil)))))
(define-test sacla-must-sequence.54 (:tag :sacla)
 (assert-true
  (let ((str (copy-seq "0123456789")))
    (and (char= (elt str 6) #\6)
         (setf (elt str 0) #\#)
         (string= str "#123456789")))))
(define-test sacla-must-sequence.55 (:tag :sacla)
 (assert-true
  (let ((list (list 0 1 2 3)))
    (and (= (elt list 2) 2)
         (setf (elt list 1) 9)
         (= (elt list 1) 9)
         (equal list '(0 9 2 3))))))
(define-test sacla-must-sequence.56 (:tag :sacla)
 (assert-true
  (let ((bv #*0101010101))
    (dotimes (i 10 t)
      (unless (= (elt bv i) (if (evenp i) 0 1))
        (return nil))))))
(define-test sacla-must-sequence.57 (:tag :sacla)
 (assert-true
  (let ((vec (vector 'a 'b 'c)))
    (and (eq (elt vec 0) 'a) (eq (elt vec 1) 'b) (eq (elt vec 2) 'c)))))
(define-test sacla-must-sequence.58 (:tag :sacla)
 (assert-true
  (let ((list (list 0 1 2 3)))
    (and (eq (fill list 'nil) list) (every 'null list)))))
(define-test sacla-must-sequence.59 (:tag :sacla)
 (assert-true
  (let ((vector (vector 'x 'y 'z)))
    (and (eq (fill vector 'a) vector)
         (every #'(lambda (arg) (eq arg 'a)) vector)))))
(define-test sacla-must-sequence.60 (:tag :sacla)
 (assert-true
  (let ((list (list 0 1 2 3)))
    (and (eq (fill list '9 :start 2) list) (equal list '(0 1 9 9))))))
(define-test sacla-must-sequence.61 (:tag :sacla)
 (assert-true
  (let ((list (list 0 1 2 3)))
    (and (eq (fill list '9 :start 1 :end 3) list) (equal list '(0 9 9 3))))))
(define-test sacla-must-sequence.62 (:tag :sacla)
 (assert-true
  (let ((list (list 0 1 2 3)))
    (and (eq (fill list '9 :start 1 :end nil) list) (equal list '(0 9 9 9))))))
(define-test sacla-must-sequence.63 (:tag :sacla)
 (assert-true
  (let ((list (list 0 1 2 3)))
    (and (eq (fill list '9 :end 1) list) (equal list '(9 1 2 3))))))
(define-test sacla-must-sequence.64 (:tag :sacla)
 (assert-true
  (let ((vector (vector 0 1 2 3)))
    (and (eq (fill vector 't :start 3) vector) (equalp vector '#(0 1 2 t))))))
(define-test sacla-must-sequence.65 (:tag :sacla)
 (assert-true
  (let ((vector (vector 0 1 2 3)))
    (and (eq (fill vector 't :start 2 :end 4) vector)
         (equalp vector '#(0 1 t t))))))
(define-test sacla-must-sequence.66 (:tag :sacla)
 (assert-true
  (let ((vector (vector 0 1 2 3)))
    (and (eq (fill vector 't :start 2 :end nil) vector)
         (equalp vector '#(0 1 t t))))))
(define-test sacla-must-sequence.67 (:tag :sacla)
 (assert-true
  (let ((vector (vector 0 1 2 3)))
    (and (eq (fill vector 't :end 3) vector) (equalp vector '#(t t t 3))))))
(define-test sacla-must-sequence.68 (:tag :sacla)
 (assert-true (null (make-sequence 'list 0))))
(define-test sacla-must-sequence.69 (:tag :sacla)
 (assert-true
  (string= (make-sequence 'string 26 :initial-element #\.)
           "..........................")))
(define-test sacla-must-sequence.70 (:tag :sacla)
 (assert-true
  (equalp (make-sequence '(vector double-float) 2 :initial-element 1.0d0)
          #(1.0d0 1.0d0))))
(define-test sacla-must-sequence.71 (:tag :sacla)
 (assert-true (equal (make-sequence 'list 3 :initial-element 'a) '(a a a))))
(define-test sacla-must-sequence.72 (:tag :sacla)
 (assert-true (equal (make-sequence 'cons 3 :initial-element 'a) '(a a a))))
(define-test sacla-must-sequence.73 (:tag :sacla)
 (assert-true (null (make-sequence 'null 0 :initial-element 'a))))
(define-test sacla-must-sequence.74 (:tag :sacla)
 (assert-true (equalp (make-sequence 'vector 3 :initial-element 'z) '#(z z z))))
(define-test sacla-must-sequence.75 (:tag :sacla)
 (assert-true
  (equalp (make-sequence '(vector * *) 3 :initial-element 'z) '#(z z z))))
(define-test sacla-must-sequence.76 (:tag :sacla)
 (assert-true
  (equalp (make-sequence '(vector t *) 3 :initial-element 'z) '#(z z z))))
(define-test sacla-must-sequence.77 (:tag :sacla)
 (assert-true
  (string= (make-sequence '(string 3) 3 :initial-element '#\a) "aaa")))
(define-test sacla-must-sequence.78 (:tag :sacla)
 (assert-true (string= (make-sequence 'string 4 :initial-element '#\z) "zzzz")))
(define-test sacla-must-sequence.79 (:tag :sacla)
 (assert-true
  (string= (make-sequence '(vector character 3) 3 :initial-element '#\a)
           "aaa")))
(define-test sacla-must-sequence.80 (:tag :sacla)
 (assert-true
  (equalp (make-sequence '(array t 1) 3 :initial-element 'z) '#(z z z))))
(define-test sacla-must-sequence.81 (:tag :sacla)
 (assert-true
  (equalp (make-sequence '(array t (3)) 3 :initial-element 'z) '#(z z z))))
(define-test sacla-must-sequence.82 (:tag :sacla)
 (assert-true (vectorp (make-sequence 'vector 10))))
(define-test sacla-must-sequence.83 (:tag :sacla)
 (assert-true (string= (subseq "012345" 2) "2345")))
(define-test sacla-must-sequence.84 (:tag :sacla)
 (assert-true (string= (subseq "012345" 3 5) "34")))
(define-test sacla-must-sequence.85 (:tag :sacla)
 (assert-true
  (let ((str (copy-seq "012345")))
    (and (setf (subseq str 4) "abc") (string= str "0123ab")))))
(define-test sacla-must-sequence.86 (:tag :sacla)
 (assert-true
  (let ((str (copy-seq "012345")))
    (setf (subseq str 0 2) "A")
    (string= str "A12345"))))
(define-test sacla-must-sequence.87 (:tag :sacla)
 (assert-true (equal (subseq '(0 1 2 3) 0) '(0 1 2 3))))
(define-test sacla-must-sequence.88 (:tag :sacla)
 (assert-true (equal (subseq '(0 1 2 3) 1) '(1 2 3))))
(define-test sacla-must-sequence.89 (:tag :sacla)
 (assert-true (equal (subseq '(0 1 2 3) 2) '(2 3))))
(define-test sacla-must-sequence.90 (:tag :sacla)
 (assert-true (equal (subseq '(0 1 2 3) 3) '(3))))
(define-test sacla-must-sequence.91 (:tag :sacla)
 (assert-true (equal (subseq '(0 1 2 3) 4) 'nil)))
(define-test sacla-must-sequence.92 (:tag :sacla)
 (assert-true (equalp (subseq #(a b c d) 0) #(a b c d))))
(define-test sacla-must-sequence.93 (:tag :sacla)
 (assert-true (equalp (subseq #(a b c d) 1) #(b c d))))
(define-test sacla-must-sequence.94 (:tag :sacla)
 (assert-true (equalp (subseq #(a b c d) 2) #(c d))))
(define-test sacla-must-sequence.95 (:tag :sacla)
 (assert-true (equalp (subseq #(a b c d) 3) #(d))))
(define-test sacla-must-sequence.96 (:tag :sacla)
 (assert-true (equalp (subseq #(a b c d) 4) #())))
(define-test sacla-must-sequence.97 (:tag :sacla)
 (assert-true (string= (subseq "0123" 0) "0123")))
(define-test sacla-must-sequence.98 (:tag :sacla)
 (assert-true (string= (subseq "0123" 1) "123")))
(define-test sacla-must-sequence.99 (:tag :sacla)
 (assert-true (string= (subseq "0123" 2) "23")))
(define-test sacla-must-sequence.100 (:tag :sacla)
 (assert-true (string= (subseq "0123" 3) "3")))
(define-test sacla-must-sequence.101 (:tag :sacla)
 (assert-true (string= (subseq "0123" 4) "")))
(define-test sacla-must-sequence.102 (:tag :sacla)
 (assert-true (equalp (subseq #*1010 0) #*1010)))
(define-test sacla-must-sequence.103 (:tag :sacla)
 (assert-true (equalp (subseq #*1010 1) #*010)))
(define-test sacla-must-sequence.104 (:tag :sacla)
 (assert-true (equalp (subseq #*1010 2) #*10)))
(define-test sacla-must-sequence.105 (:tag :sacla)
 (assert-true (equalp (subseq #*1010 3) #*0)))
(define-test sacla-must-sequence.106 (:tag :sacla)
 (assert-true (equalp (subseq #*1010 4) #*)))
(define-test sacla-must-sequence.107 (:tag :sacla)
 (assert-true (equal (subseq '(0 1 2 3) 0 4) '(0 1 2 3))))
(define-test sacla-must-sequence.108 (:tag :sacla)
 (assert-true (equal (subseq '(0 1 2 3) 0 nil) '(0 1 2 3))))
(define-test sacla-must-sequence.109 (:tag :sacla)
 (assert-true
  (let* ((list0 '(0 1 2 3)) (list (subseq list0 0 4)))
    (and (not (eq list0 list)) (equal list0 list)))))
(define-test sacla-must-sequence.110 (:tag :sacla)
 (assert-true
  (let* ((list0 '(0 1 2 3)) (list (subseq list0 0 nil)))
    (and (not (eq list0 list)) (equal list0 list)))))
(define-test sacla-must-sequence.111 (:tag :sacla)
 (assert-true (equal (subseq '(0 1 2 3) 1 3) '(1 2))))
(define-test sacla-must-sequence.112 (:tag :sacla)
 (assert-true (equal (subseq '(0 1 2 3) 2 2) 'nil)))
(define-test sacla-must-sequence.113 (:tag :sacla)
 (assert-true (equal (subseq '(0 1 2 3) 0 0) 'nil)))
(define-test sacla-must-sequence.114 (:tag :sacla)
 (assert-true (equal (subseq '(0 1 2 3) 1 1) 'nil)))
(define-test sacla-must-sequence.115 (:tag :sacla)
 (assert-true (equal (subseq '(0 1 2 3) 2 2) 'nil)))
(define-test sacla-must-sequence.116 (:tag :sacla)
 (assert-true (equal (subseq '(0 1 2 3) 3 3) 'nil)))
(define-test sacla-must-sequence.117 (:tag :sacla)
 (assert-true (equal (subseq '(0 1 2 3) 4 4) 'nil)))
(define-test sacla-must-sequence.118 (:tag :sacla)
 (assert-true
  (let ((list (list 0 1 2 3 4 5 6 7)))
    (setf (subseq list 0) '(a b c d))
    (equal list '(a b c d 4 5 6 7)))))
(define-test sacla-must-sequence.119 (:tag :sacla)
 (assert-true
  (let ((list (list 0 1 2 3)))
    (setf (subseq list 0) '(a b c d))
    (equal list '(a b c d)))))
(define-test sacla-must-sequence.120 (:tag :sacla)
 (assert-true
  (let ((list (list 0 1 2 3)))
    (setf (subseq list 2) '(a b c d))
    (equal list '(0 1 a b)))))
(define-test sacla-must-sequence.121 (:tag :sacla)
 (assert-true
  (let ((list (list 0 1 2 3)))
    (setf (subseq list 2 nil) '(a b c d))
    (equal list '(0 1 a b)))))
(define-test sacla-must-sequence.122 (:tag :sacla)
 (assert-true
  (let ((list (list 0 1 2 3)))
    (setf (subseq list 1 3) '(a b c d))
    (equal list '(0 a b 3)))))
(define-test sacla-must-sequence.123 (:tag :sacla)
 (assert-true
  (let ((list (list 0 1 2 3)))
    (setf (subseq list 0) 'nil)
    (equal list '(0 1 2 3)))))
(define-test sacla-must-sequence.124 (:tag :sacla)
 (assert-true
  (let ((list 'nil))
    (setf (subseq list 0) '(a b c d e))
    (null list))))
(define-test sacla-must-sequence.125 (:tag :sacla)
 (assert-true
  (let ((list '(0 1 2 3)))
    (setf (subseq list 0 0) '(a b c d e))
    (equal list '(0 1 2 3)))))
(define-test sacla-must-sequence.126 (:tag :sacla)
 (assert-true
  (let ((list '(0 1 2 3)))
    (setf (subseq list 1 1) '(a b c d e))
    (equal list '(0 1 2 3)))))
(define-test sacla-must-sequence.127 (:tag :sacla)
 (assert-true
  (let ((list '(0 1 2 3)))
    (setf (subseq list 2 2) '(a b c d e))
    (equal list '(0 1 2 3)))))
(define-test sacla-must-sequence.128 (:tag :sacla)
 (assert-true
  (let ((list '(0 1 2 3)))
    (setf (subseq list 3 3) '(a b c d e))
    (equal list '(0 1 2 3)))))
(define-test sacla-must-sequence.129 (:tag :sacla)
 (assert-true
  (let ((list '(0 1 2 3)))
    (setf (subseq list 4 4) '(a b c d e))
    (equal list '(0 1 2 3)))))
(define-test sacla-must-sequence.130 (:tag :sacla)
 (assert-true
  (let ((list (list 0 1 2 3 4 5 6 7)))
    (setf (subseq list 0) #(a b c d))
    (equal list '(a b c d 4 5 6 7)))))
(define-test sacla-must-sequence.131 (:tag :sacla)
 (assert-true
  (let ((list (list 0 1 2 3)))
    (setf (subseq list 0) #(a b c d))
    (equal list '(a b c d)))))
(define-test sacla-must-sequence.132 (:tag :sacla)
 (assert-true
  (let ((list (list 0 1 2 3)))
    (setf (subseq list 2) #(a b c d))
    (equal list '(0 1 a b)))))
(define-test sacla-must-sequence.133 (:tag :sacla)
 (assert-true
  (let ((list (list 0 1 2 3)))
    (setf (subseq list 2 nil) #(a b c d))
    (equal list '(0 1 a b)))))
(define-test sacla-must-sequence.134 (:tag :sacla)
 (assert-true
  (let ((list (list 0 1 2 3)))
    (setf (subseq list 1 3) #(a b c d))
    (equal list '(0 a b 3)))))
(define-test sacla-must-sequence.135 (:tag :sacla)
 (assert-true
  (let ((list (list 0 1 2 3)))
    (setf (subseq list 0) #())
    (equal list '(0 1 2 3)))))
(define-test sacla-must-sequence.136 (:tag :sacla)
 (assert-true
  (let ((list (list 0 1 2 3 4 5 6 7)))
    (setf (subseq list 0) "abcd")
    (equal list '(#\a #\b #\c #\d 4 5 6 7)))))
(define-test sacla-must-sequence.137 (:tag :sacla)
 (assert-true
  (let ((list (list 0 1 2 3)))
    (setf (subseq list 0) "abcd")
    (equal list '(#\a #\b #\c #\d)))))
(define-test sacla-must-sequence.138 (:tag :sacla)
 (assert-true
  (let ((list (list 0 1 2 3)))
    (setf (subseq list 2) "abcd")
    (equal list '(0 1 #\a #\b)))))
(define-test sacla-must-sequence.139 (:tag :sacla)
 (assert-true
  (let ((list (list 0 1 2 3)))
    (setf (subseq list 2 nil) "abcd")
    (equal list '(0 1 #\a #\b)))))
(define-test sacla-must-sequence.140 (:tag :sacla)
 (assert-true
  (let ((list (list 0 1 2 3)))
    (setf (subseq list 1 3) "abcd")
    (equal list '(0 #\a #\b 3)))))
(define-test sacla-must-sequence.141 (:tag :sacla)
 (assert-true
  (let ((list (list 0 1 2 3)))
    (setf (subseq list 0) "")
    (equal list '(0 1 2 3)))))
(define-test sacla-must-sequence.142 (:tag :sacla)
 (assert-true (equalp (subseq #(0 1 2 3) 0 4) #(0 1 2 3))))
(define-test sacla-must-sequence.143 (:tag :sacla)
 (assert-true (equalp (subseq #(0 1 2 3) 0 nil) #(0 1 2 3))))
(define-test sacla-must-sequence.144 (:tag :sacla)
 (assert-true
  (let* ((vec0 #(0 1 2 3)) (vec (subseq vec0 0 4)))
    (and (not (eq vec0 vec)) (equalp vec0 vec)))))
(define-test sacla-must-sequence.145 (:tag :sacla)
 (assert-true
  (let* ((vec0 #(0 1 2 3)) (vec (subseq vec0 0 nil)))
    (and (not (eq vec0 vec)) (equalp vec0 vec)))))
(define-test sacla-must-sequence.146 (:tag :sacla)
 (assert-true (equalp (subseq #(0 1 2 3) 1 3) #(1 2))))
(define-test sacla-must-sequence.147 (:tag :sacla)
 (assert-true (equalp (subseq #(0 1 2 3) 2 2) #())))
(define-test sacla-must-sequence.148 (:tag :sacla)
 (assert-true (equalp (subseq #(0 1 2 3) 0 0) #())))
(define-test sacla-must-sequence.149 (:tag :sacla)
 (assert-true (equalp (subseq #(0 1 2 3) 1 1) #())))
(define-test sacla-must-sequence.150 (:tag :sacla)
 (assert-true (equalp (subseq #(0 1 2 3) 2 2) #())))
(define-test sacla-must-sequence.151 (:tag :sacla)
 (assert-true (equalp (subseq #(0 1 2 3) 3 3) #())))
(define-test sacla-must-sequence.152 (:tag :sacla)
 (assert-true (equalp (subseq #(0 1 2 3) 4 4) #())))
(define-test sacla-must-sequence.153 (:tag :sacla)
 (assert-true
  (let ((vec (vector 0 1 2 3 4 5 6 7)))
    (setf (subseq vec 0) #(a b c d))
    (equalp vec #(a b c d 4 5 6 7)))))
(define-test sacla-must-sequence.154 (:tag :sacla)
 (assert-true
  (let ((vec (vector 0 1 2 3)))
    (setf (subseq vec 0) #(a b c d))
    (equalp vec #(a b c d)))))
(define-test sacla-must-sequence.155 (:tag :sacla)
 (assert-true
  (let ((vec (vector 0 1 2 3)))
    (setf (subseq vec 2) #(a b c d))
    (equalp vec #(0 1 a b)))))
(define-test sacla-must-sequence.156 (:tag :sacla)
 (assert-true
  (let ((vec (vector 0 1 2 3)))
    (setf (subseq vec 1 3) #(a b c d))
    (equalp vec #(0 a b 3)))))
(define-test sacla-must-sequence.157 (:tag :sacla)
 (assert-true
  (let ((vec (vector 0 1 2 3)))
    (setf (subseq vec 0) #())
    (equalp vec #(0 1 2 3)))))
(define-test sacla-must-sequence.158 (:tag :sacla)
 (assert-true
  (let ((vec (vector)))
    (setf (subseq vec 0) #(a b c d e))
    (equalp vec #()))))
(define-test sacla-must-sequence.159 (:tag :sacla)
 (assert-true
  (let ((vec (vector 0 1 2 3)))
    (setf (subseq vec 0 0) #(a b c d e))
    (equalp vec #(0 1 2 3)))))
(define-test sacla-must-sequence.160 (:tag :sacla)
 (assert-true
  (let ((vec (vector 0 1 2 3)))
    (setf (subseq vec 1 1) #(a b c d e))
    (equalp vec #(0 1 2 3)))))
(define-test sacla-must-sequence.161 (:tag :sacla)
 (assert-true
  (let ((vec (vector 0 1 2 3)))
    (setf (subseq vec 2 2) #(a b c d e))
    (equalp vec #(0 1 2 3)))))
(define-test sacla-must-sequence.162 (:tag :sacla)
 (assert-true
  (let ((vec (vector 0 1 2 3)))
    (setf (subseq vec 3 3) #(a b c d e))
    (equalp vec #(0 1 2 3)))))
(define-test sacla-must-sequence.163 (:tag :sacla)
 (assert-true
  (let ((vec (vector 0 1 2 3)))
    (setf (subseq vec 4 4) #(a b c d e))
    (equalp vec #(0 1 2 3)))))
(define-test sacla-must-sequence.164 (:tag :sacla)
 (assert-true
  (let ((vec (vector 0 1 2 3 4 5 6 7)))
    (setf (subseq vec 0) #(a b c d))
    (equalp vec #(a b c d 4 5 6 7)))))
(define-test sacla-must-sequence.165 (:tag :sacla)
 (assert-true
  (let ((vec (vector 0 1 2 3)))
    (setf (subseq vec 0) #(a b c d))
    (equalp vec #(a b c d)))))
(define-test sacla-must-sequence.166 (:tag :sacla)
 (assert-true
  (let ((vec (vector 0 1 2 3)))
    (setf (subseq vec 2) #(a b c d))
    (equalp vec #(0 1 a b)))))
(define-test sacla-must-sequence.167 (:tag :sacla)
 (assert-true
  (let ((vec (vector 0 1 2 3)))
    (setf (subseq vec 2 nil) #(a b c d))
    (equalp vec #(0 1 a b)))))
(define-test sacla-must-sequence.168 (:tag :sacla)
 (assert-true
  (let ((vec (vector 0 1 2 3)))
    (setf (subseq vec 1 3) #(a b c d))
    (equalp vec #(0 a b 3)))))
(define-test sacla-must-sequence.169 (:tag :sacla)
 (assert-true
  (let ((vec (vector 0 1 2 3)))
    (setf (subseq vec 0) #())
    (equalp vec #(0 1 2 3)))))
(define-test sacla-must-sequence.170 (:tag :sacla)
 (assert-true
  (handler-case
      (progn
        (map 'symbol #'+ '(0 1) '(1 0)))
    (type-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-sequence.171 (:tag :sacla)
 (assert-true
  (handler-case
      (progn
        (map 'hash-table #'+ '(0 1) '(1 0)))
    (type-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-sequence.172 (:tag :sacla)
 (assert-true
  (string=
   (map 'string
        #'(lambda (x y) (char "01234567890ABCDEF" (mod (+ x y) 16)))
        '(1 2 3 4)
        '(10 9 8 7))
   "AAAA")))
(define-test sacla-must-sequence.173 (:tag :sacla)
 (assert-true
  (let ((seq (list "lower" "UPPER" "" "123")))
    (and (null (map nil #'nstring-upcase seq))
         (equal seq '("LOWER" "UPPER" "" "123"))))))
(define-test sacla-must-sequence.174 (:tag :sacla)
 (assert-true (equal (map 'list #'- '(1 2 3 4)) '(-1 -2 -3 -4))))
(define-test sacla-must-sequence.175 (:tag :sacla)
 (assert-true
  (string= (map 'string #'(lambda (x) (if (oddp x) #\1 #\0)) '(1 2 3 4))
           "1010")))
(define-test sacla-must-sequence.176 (:tag :sacla)
 (assert-true (equal (map 'list '+ '(0 1) '(1 0)) '(1 1))))
(define-test sacla-must-sequence.177 (:tag :sacla)
 (assert-true (equal (map 'list '- '(0 1) '(1 0)) '(-1 1))))
(define-test sacla-must-sequence.178 (:tag :sacla)
 (assert-true
  (every 'null
         (list (map 'list #'+ 'nil)
               (map 'list #'+ 'nil 'nil)
               (map 'list #'+ 'nil 'nil 'nil)
               (map 'list #'+ 'nil 'nil 'nil 'nil)
               (map 'list #'+ 'nil 'nil 'nil 'nil 'nil)))))
(define-test sacla-must-sequence.179 (:tag :sacla)
 (assert-true
  (every 'null
         (list (map 'list #'+ 'nil)
               (map 'list #'+ #() 'nil)
               (map 'list #'+ 'nil #() 'nil)
               (map 'list #'+ #() 'nil #() 'nil)
               (map 'list #'+ 'nil #() 'nil #() 'nil)))))
(define-test sacla-must-sequence.180 (:tag :sacla)
 (assert-true (equal (map 'list #'+ '(0 1 2)) '(0 1 2))))
(define-test sacla-must-sequence.181 (:tag :sacla)
 (assert-true (equal (map 'list #'+ '(0 1 2) '(1 2 3)) '(1 3 5))))
(define-test sacla-must-sequence.182 (:tag :sacla)
 (assert-true (equal (map 'list #'+ '(0 1 2) '(1 2 3) '(2 3 4)) '(3 6 9))))
(define-test sacla-must-sequence.183 (:tag :sacla)
 (assert-true
  (equal (map 'list #'+ '(0 1 2) '(1 2 3) '(2 3 4) '(3 4 5)) '(6 10 14))))
(define-test sacla-must-sequence.184 (:tag :sacla)
 (assert-true (equal (map 'list #'+ '(1 2) '(1 2 3)) '(2 4))))
(define-test sacla-must-sequence.185 (:tag :sacla)
 (assert-true (equal (map 'list #'+ '(0 1 2) '(2 3) '(2 3 4)) '(4 7))))
(define-test sacla-must-sequence.186 (:tag :sacla)
 (assert-true (equal (map 'list #'+ '(0 1 2) '(1 2 3) '(2) '(3 4 5)) '(6))))
(define-test sacla-must-sequence.187 (:tag :sacla)
 (assert-true
  (equal (map 'list #'+ '(0 1 2) '(1 2 3) '(2 3 4) '(3 4 5) 'nil) 'nil)))
(define-test sacla-must-sequence.188 (:tag :sacla)
 (assert-true (equal (map 'cons #'+ '(0 1 2) '(2 1 0)) '(2 2 2))))
(define-test sacla-must-sequence.189 (:tag :sacla)
 (assert-true (equal (map '(cons number cons) #'+ '(0 1 2) '(2 1 0)) '(2 2 2))))
(define-test sacla-must-sequence.190 (:tag :sacla)
 (assert-true
  (equal (map '(cons number (cons number *)) #'+ '(0 1 2) '(2 1 0)) '(2 2 2))))
(define-test sacla-must-sequence.191 (:tag :sacla)
 (assert-true (null (map 'null #'+ 'nil))))
(define-test sacla-must-sequence.192 (:tag :sacla)
 (assert-true (equalp (map 'vector #'+ #()) #())))
(define-test sacla-must-sequence.193 (:tag :sacla)
 (assert-true (equalp (map 'vector #'+ #() #()) #())))
(define-test sacla-must-sequence.194 (:tag :sacla)
 (assert-true (equalp (map 'vector #'+ #() #() #()) #())))
(define-test sacla-must-sequence.195 (:tag :sacla)
 (assert-true (equalp (map 'vector #'+ #() #() #() #()) #())))
(define-test sacla-must-sequence.196 (:tag :sacla)
 (assert-true (equalp (map 'vector #'+ #() #() #() #() #()) #())))
(define-test sacla-must-sequence.197 (:tag :sacla)
 (assert-true (equalp (map 'vector #'+ 'nil #()) #())))
(define-test sacla-must-sequence.198 (:tag :sacla)
 (assert-true (equalp (map 'vector #'+ 'nil #() "") #())))
(define-test sacla-must-sequence.199 (:tag :sacla)
 (assert-true (equalp (map 'vector #'+ '(0 1 2)) #(0 1 2))))
(define-test sacla-must-sequence.200 (:tag :sacla)
 (assert-true (equalp (map 'vector #'+ '(0 1 2) #(1 2 3)) #(1 3 5))))
(define-test sacla-must-sequence.201 (:tag :sacla)
 (assert-true (equalp (map 'vector #'+ #(0 1 2) '(1 2 3) #(2 3 4)) #(3 6 9))))
(define-test sacla-must-sequence.202 (:tag :sacla)
 (assert-true
  (equalp (map 'vector #'+ '(0 1 2) #(1 2 3) '(2 3 4) #(3 4 5)) #(6 10 14))))
(define-test sacla-must-sequence.203 (:tag :sacla)
 (assert-true (equalp (map 'vector #'+ '(1 2) '(1 2 3)) #(2 4))))
(define-test sacla-must-sequence.204 (:tag :sacla)
 (assert-true (equalp (map 'vector #'+ '(0 1 2) '(2 3) '(2 3 4)) #(4 7))))
(define-test sacla-must-sequence.205 (:tag :sacla)
 (assert-true (equalp (map 'vector #'+ '(0 1 2) '(1 2 3) '(2) '(3 4 5)) #(6))))
(define-test sacla-must-sequence.206 (:tag :sacla)
 (assert-true
  (equalp (map 'vector #'+ '(0 1 2) '(1 2 3) '(2 3 4) '(3 4 5) 'nil) #())))
(define-test sacla-must-sequence.207 (:tag :sacla)
 (assert-true (equalp (map 'vector #'+ #(1 2) #(1 2 3)) #(2 4))))
(define-test sacla-must-sequence.208 (:tag :sacla)
 (assert-true (equalp (map 'vector #'+ #(0 1 2) #(2 3) #(2 3 4)) #(4 7))))
(define-test sacla-must-sequence.209 (:tag :sacla)
 (assert-true (equalp (map 'vector #'+ #(0 1 2) '(1 2 3) #(2) '(3 4 5)) #(6))))
(define-test sacla-must-sequence.210 (:tag :sacla)
 (assert-true
  (equalp (map 'vector #'+ '(0 1 2) #(1 2 3) '(2 3 4) '(3 4 5) 'nil) #())))
(define-test sacla-must-sequence.211 (:tag :sacla)
 (assert-true
  (string= (map 'string #'(lambda (&rest rest) (char-upcase (car rest))) "")
           "")))
(define-test sacla-must-sequence.212 (:tag :sacla)
 (assert-true
  (string= (map 'string #'(lambda (&rest rest) (char-upcase (car rest))) "" "")
           "")))
(define-test sacla-must-sequence.213 (:tag :sacla)
 (assert-true
  (string=
   (map 'string #'(lambda (&rest rest) (char-upcase (car rest))) "" "" "")
   "")))
(define-test sacla-must-sequence.214 (:tag :sacla)
 (assert-true
  (string=
   (map 'string #'(lambda (&rest rest) (char-upcase (car rest))) "" "" "" "")
   "")))
(define-test sacla-must-sequence.215 (:tag :sacla)
 (assert-true
  (string=
   (map 'string
        #'(lambda (&rest rest) (char-upcase (car rest)))
        ""
        ""
        ""
        ""
        "")
   "")))
(define-test sacla-must-sequence.216 (:tag :sacla)
 (assert-true
  (string= (map 'string #'(lambda (&rest rest) (char-upcase (car rest))) "")
           "")))
(define-test sacla-must-sequence.217 (:tag :sacla)
 (assert-true
  (string=
   (map 'string #'(lambda (&rest rest) (char-upcase (car rest))) "" 'nil)
   "")))
(define-test sacla-must-sequence.218 (:tag :sacla)
 (assert-true
  (string=
   (map 'string #'(lambda (&rest rest) (char-upcase (car rest))) "" #() 'nil)
   "")))
(define-test sacla-must-sequence.219 (:tag :sacla)
 (assert-true
  (string=
   (map 'string
        #'(lambda (&rest rest) (char-upcase (car rest)))
        'nil
        'nil
        ""
        "")
   "")))
(define-test sacla-must-sequence.220 (:tag :sacla)
 (assert-true
  (string=
   (map 'string
        #'(lambda (&rest rest) (char-upcase (car rest)))
        #()
        #()
        #()
        #()
        #())
   "")))
(define-test sacla-must-sequence.221 (:tag :sacla)
 (assert-true
  (string=
   (map 'string #'(lambda (a b) (if (char< a b) b a)) "axbycz" "xaybzc")
   "xxyyzz")))
(define-test sacla-must-sequence.222 (:tag :sacla)
 (assert-true
  (string= (map 'string #'(lambda (a b) (if (char< a b) b a)) "axbycz" "xayb")
           "xxyy")))
(define-test sacla-must-sequence.223 (:tag :sacla)
 (assert-true
  (string=
   (map 'string
        #'(lambda (&rest rest) (if (zerop (apply #'logand rest)) #\0 #\1))
        '(0 1 0 1)
        #*1010101)
   "0000")))
(define-test sacla-must-sequence.224 (:tag :sacla)
 (assert-true
  (string=
   (map 'string
        #'(lambda (&rest rest) (if (zerop (apply #'logand rest)) #\0 #\1))
        #*1111
        #*1010101
        #*001)
   "001")))
(define-test sacla-must-sequence.225 (:tag :sacla)
 (assert-true
  (string=
   (map 'string
        #'(lambda (&rest rest) (if (zerop (apply #'logand rest)) #\0 #\1))
        #*1111
        #*1010101
        #*0)
   "0")))
(define-test sacla-must-sequence.226 (:tag :sacla)
 (assert-true
  (string=
   (map 'string
        #'(lambda (&rest rest) (if (zerop (apply #'logand rest)) #\0 #\1))
        #*1111
        #*1000
        #*1011)
   "1000")))
(define-test sacla-must-sequence.227 (:tag :sacla)
 (assert-true
  (let ((list nil))
    (and
     (null
      (map nil
           #'(lambda (&rest rest) (setq list (cons (apply #'+ rest) list)))
           '(0 1 2 3)
           '(1 2 3 4)))
     (equal list '(7 5 3 1))))))
(define-test sacla-must-sequence.228 (:tag :sacla)
 (assert-true
  (let ((list nil))
    (and
     (null
      (map nil
           #'(lambda (&rest rest) (setq list (cons (apply #'+ rest) list)))
           '(0 1 2 3)
           '(1 2 3 4)
           '(2 3 4 5)))
     (equal list (reverse '(3 6 9 12)))))))
(define-test sacla-must-sequence.229 (:tag :sacla)
 (assert-true
  (let ((list nil))
    (and
     (null
      (map nil
           #'(lambda (&rest rest) (setq list (cons (apply #'+ rest) list)))
           '(0 1 2 3)
           '(1)
           '(2 3 4 5)))
     (equal list '(3))))))
(define-test sacla-must-sequence.230 (:tag :sacla)
 (assert-true (equalp (map '(vector * 2) #'+ #*01 #*10) #(1 1))))
(define-test sacla-must-sequence.231 (:tag :sacla)
 (assert-true (equalp (map '(simple-vector 2) #'+ #*01 #*10) #(1 1))))
(define-test sacla-must-sequence.232 (:tag :sacla)
 (assert-true (equalp (map '(array * 1) #'+ #*01 #*10) #(1 1))))
(define-test sacla-must-sequence.233 (:tag :sacla)
 (assert-true (equalp (map '(simple-array * 1) #'+ #*01 #*10) #(1 1))))
(define-test sacla-must-sequence.234 (:tag :sacla)
 (assert-true (equalp (map '(array * (2)) #'+ #*01 #*10) #(1 1))))
(define-test sacla-must-sequence.235 (:tag :sacla)
 (assert-true (equalp (map '(simple-array * (2)) #'+ #*01 #*10) #(1 1))))
(define-test sacla-must-sequence.236 (:tag :sacla)
 (assert-true (string= (map 'string #'char-upcase "abc") "ABC")))
(define-test sacla-must-sequence.237 (:tag :sacla)
 (assert-true (string= (map 'base-string #'char-upcase "abc") "ABC")))
(define-test sacla-must-sequence.238 (:tag :sacla)
 (assert-true (string= (map 'simple-string #'char-upcase "abc") "ABC")))
(define-test sacla-must-sequence.239 (:tag :sacla)
 (assert-true (string= (map '(string 3) #'char-upcase "abc") "ABC")))
(define-test sacla-must-sequence.240 (:tag :sacla)
 (assert-true (string= (map '(base-string 3) #'char-upcase "abc") "ABC")))
(define-test sacla-must-sequence.241 (:tag :sacla)
 (assert-true (string= (map '(simple-string 3) #'char-upcase "abc") "ABC")))
(define-test sacla-must-sequence.242 (:tag :sacla)
 (assert-true (string= (map '(vector character) #'char-upcase "abc") "ABC")))
(define-test sacla-must-sequence.243 (:tag :sacla)
 (assert-true (string= (map '(vector character 3) #'char-upcase "abc") "ABC")))
(define-test sacla-must-sequence.244 (:tag :sacla)
 (assert-true (string= (map '(vector base-char) #'char-upcase "abc") "ABC")))
(define-test sacla-must-sequence.245 (:tag :sacla)
 (assert-true (string= (map '(vector base-char 3) #'char-upcase "abc") "ABC")))
(define-test sacla-must-sequence.246 (:tag :sacla)
 (assert-true
  (string= (map '(vector standard-char) #'char-upcase "abc") "ABC")))
(define-test sacla-must-sequence.247 (:tag :sacla)
 (assert-true
  (string= (map '(vector standard-char 3) #'char-upcase "abc") "ABC")))
(define-test sacla-must-sequence.248 (:tag :sacla)
 (assert-true (string= (map '(array character 1) #'char-upcase "abc") "ABC")))
(define-test sacla-must-sequence.249 (:tag :sacla)
 (assert-true (string= (map '(array character (3)) #'char-upcase "abc") "ABC")))
(define-test sacla-must-sequence.250 (:tag :sacla)
 (assert-true (string= (map '(array base-char 1) #'char-upcase "abc") "ABC")))
(define-test sacla-must-sequence.251 (:tag :sacla)
 (assert-true (string= (map '(array base-char (3)) #'char-upcase "abc") "ABC")))
(define-test sacla-must-sequence.252 (:tag :sacla)
 (assert-true
  (string= (map '(array standard-char 1) #'char-upcase "abc") "ABC")))
(define-test sacla-must-sequence.253 (:tag :sacla)
 (assert-true
  (string= (map '(array standard-char (3)) #'char-upcase "abc") "ABC")))
(define-test sacla-must-sequence.254 (:tag :sacla)
 (assert-true (equalp (map 'bit-vector #'logand '(0 1 0 1) #*1010) #*0000)))
(define-test sacla-must-sequence.255 (:tag :sacla)
 (assert-true
  (equalp (map 'simple-bit-vector #'logand '(0 1 0 1) #*1010) #*0000)))
(define-test sacla-must-sequence.256 (:tag :sacla)
 (assert-true (equalp (map '(bit-vector 4) #'logand '(0 1 0 1) #*1010) #*0000)))
(define-test sacla-must-sequence.257 (:tag :sacla)
 (assert-true
  (equalp (map '(simple-bit-vector 4) #'logand '(0 1 0 1) #*1010) #*0000)))
(define-test sacla-must-sequence.258 (:tag :sacla)
 (assert-true (equalp (map '(array bit 1) #'logand '(0 1 0 1) #*1010) #*0000)))
(define-test sacla-must-sequence.259 (:tag :sacla)
 (assert-true
  (equalp (map '(array bit (4)) #'logand '(0 1 0 1) #*1010) #*0000)))
(define-test sacla-must-sequence.260 (:tag :sacla)
 (assert-true
  (equalp (map '(simple-array bit 1) #'logand '(0 1 0 1) #*1010) #*0000)))
(define-test sacla-must-sequence.261 (:tag :sacla)
 (assert-true
  (equalp (map '(simple-array bit (4)) #'logand '(0 1 0 1) #*1010) #*0000)))
(define-test sacla-must-sequence.262 (:tag :sacla)
 (assert-true (equalp (map '(vector bit) #'logand '(0 1 0 1) #*1010) #*0000)))
(define-test sacla-must-sequence.263 (:tag :sacla)
 (assert-true (equalp (map '(vector bit 4) #'logand '(0 1 0 1) #*1010) #*0000)))
(define-test sacla-must-sequence.264 (:tag :sacla)
 (assert-true (equal (map 'list #'+ '(0 1 2 3) #(3 2 1 0) #*0101) '(3 4 3 4))))
(define-test sacla-must-sequence.265 (:tag :sacla)
 (assert-true
  (equalp (map 'vector #'+ '(0 1 2 3) #(3 2 1 0) #*0101) #(3 4 3 4))))
(define-test sacla-must-sequence.266 (:tag :sacla)
 (assert-true
  (let ((a (list 1 2 3 4)) (b (list 10 10 10 10)))
    (and (equal (map-into a #'+ a b) '(11 12 13 14))
         (equal a '(11 12 13 14))
         (equal b '(10 10 10 10))))))
(define-test sacla-must-sequence.267 (:tag :sacla)
 (assert-true
  (let ((a '(11 12 13 14)) (k '(one two three)))
    (equal (map-into a #'cons k a) '((one . 11) (two . 12) (three . 13) 14)))))
(define-test sacla-must-sequence.268 (:tag :sacla)
 (assert-true (null (map-into nil 'identity))))
(define-test sacla-must-sequence.269 (:tag :sacla)
 (assert-true (null (map-into nil #'identity))))
(define-test sacla-must-sequence.270 (:tag :sacla)
 (assert-true (null (map-into nil #'identity 'nil))))
(define-test sacla-must-sequence.271 (:tag :sacla)
 (assert-true (null (map-into nil #'identity '(0 1 2) '(9 8 7)))))
(define-test sacla-must-sequence.272 (:tag :sacla)
 (assert-true
  (let ((list (list 0 1 2)))
    (and (eq (map-into list 'identity) list) (equal list '(0 1 2))))))
(define-test sacla-must-sequence.273 (:tag :sacla)
 (assert-true
  (let ((list (list 0 1 2)))
    (and (eq (map-into list 'identity 'nil) list) (equal list '(0 1 2))))))
(define-test sacla-must-sequence.274 (:tag :sacla)
 (assert-true
  (let ((vec (vector 0 1 2)))
    (and (eq (map-into vec 'identity) vec) (equalp vec #(0 1 2))))))
(define-test sacla-must-sequence.275 (:tag :sacla)
 (assert-true
  (let ((vec (vector 0 1 2)))
    (and (eq (map-into vec 'identity #()) vec) (equalp vec #(0 1 2))))))
(define-test sacla-must-sequence.276 (:tag :sacla)
 (assert-true
  (let ((vec (vector 0 1 2)))
    (and (eq (map-into vec #'+ #() 'nil #()) vec) (equalp vec #(0 1 2))))))
(define-test sacla-must-sequence.277 (:tag :sacla)
 (assert-true (equal (map-into (list nil nil) '+ '(0 1) '(1 0)) '(1 1))))
(define-test sacla-must-sequence.278 (:tag :sacla)
 (assert-true (equal (map-into (list nil nil) '- '(0 1) '(1 0)) '(-1 1))))
(define-test sacla-must-sequence.279 (:tag :sacla)
 (assert-true
  (let ((list (make-list 3 :initial-element nil)))
    (and (eq (map-into list #'+ '(0 1 2)) list) (equal list '(0 1 2))))))
(define-test sacla-must-sequence.280 (:tag :sacla)
 (assert-true
  (let ((list (make-list 3 :initial-element nil)))
    (and (eq (map-into list #'+ '(0 1 2) '(1 2 3)) list)
         (equal list '(1 3 5))))))
(define-test sacla-must-sequence.281 (:tag :sacla)
 (assert-true
  (let ((list (make-list 3 :initial-element nil)))
    (and (eq (map-into list #'+ '(0 1 2) '(1 2 3) '(2 3 4)) list)
         (equal list '(3 6 9))))))
(define-test sacla-must-sequence.282 (:tag :sacla)
 (assert-true
  (let ((list (make-list 3 :initial-element nil)))
    (and (eq (map-into list #'+ '(1 2) '(1 2 3)) list)
         (equal list '(2 4 nil))))))
(define-test sacla-must-sequence.283 (:tag :sacla)
 (assert-true
  (let ((list (make-list 1 :initial-element nil)))
    (and (eq (map-into list #'+ '(1 2 3) '(1 2 3)) list) (equal list '(2))))))
(define-test sacla-must-sequence.284 (:tag :sacla)
 (assert-true
  (let ((list (make-list 3 :initial-element nil)))
    (and (eq (map-into list #'+ '(1 2 3 4) '(1 2 3) '(0)) list)
         (equal list '(2 nil nil))))))
(define-test sacla-must-sequence.285 (:tag :sacla)
 (assert-true
  (let ((vec (make-sequence 'vector 3 :initial-element nil)))
    (and (eq (map-into vec #'+ '(0 1 2)) vec) (equalp vec #(0 1 2))))))
(define-test sacla-must-sequence.286 (:tag :sacla)
 (assert-true
  (let ((vec (make-sequence 'vector 3 :initial-element nil)))
    (and (eq (map-into vec #'+ '(0 1 2) #(1 2 3)) vec) (equalp vec #(1 3 5))))))
(define-test sacla-must-sequence.287 (:tag :sacla)
 (assert-true
  (let ((vec (make-sequence 'vector 3 :initial-element nil)))
    (and (eq (map-into vec #'+ '(0 1 2) '(1 2 3) #(2 3 4)) vec)
         (equalp vec #(3 6 9))))))
(define-test sacla-must-sequence.288 (:tag :sacla)
 (assert-true
  (let ((vec (make-sequence 'vector 3 :initial-element nil)))
    (and (eq (map-into vec #'+ '(1 2) #(1 2 3)) vec) (equalp vec #(2 4 nil))))))
(define-test sacla-must-sequence.289 (:tag :sacla)
 (assert-true
  (let ((vec (make-sequence 'vector 1 :initial-element nil)))
    (and (eq (map-into vec #'+ '(1 2) #(1 2 3)) vec) (equalp vec #(2))))))
(define-test sacla-must-sequence.290 (:tag :sacla)
 (assert-true
  (let ((vec (make-sequence 'vector 3 :initial-element nil)))
    (and (eq (map-into vec #'+ '(1 2 3 4) #(1 2 3) '(0)) vec)
         (equalp vec #(2 nil nil))))))
(define-test sacla-must-sequence.291 (:tag :sacla)
 (assert-true
  (let ((str
         (make-array 10
                     :element-type 'character
                     :initial-contents "0123456789"
                     :fill-pointer 3)))
    (and (eq (map-into str #'char-upcase "abcde") str)
         (string= str "ABCDE")
         (= (fill-pointer str) 5)))))
(define-test sacla-must-sequence.292 (:tag :sacla)
 (assert-true
  (let ((vec (make-array 5 :initial-contents #(0 1 2 3 4) :fill-pointer 3)))
    (and
     (eq (map-into vec #'+ '(0 1 2 3 4 5 6 7 8 9) '(9 8 7 6 5 4 3 2 1 0)) vec)
     (equalp vec #(9 9 9 9 9))))))
(define-test sacla-must-sequence.293 (:tag :sacla)
 (assert-true
  (let ((vec (make-array 5 :initial-contents #(0 1 2 3 4) :fill-pointer 3)))
    (and (eq (map-into vec #'+ '(0 1) '(9 8 7 6 5 4 3 2 1 0)) vec)
         (equalp vec #(9 9))))))
(define-test sacla-must-sequence.294 (:tag :sacla)
 (assert-true
  (let ((vec
         (make-array 5
                     :element-type 'bit
                     :initial-contents #(1 1 1 1 1)
                     :fill-pointer 3)))
    (and (eq (map-into vec #'logand '(0 1) '(1 0 1 0 1 0)) vec)
         (equalp vec #*00)))))
(define-test sacla-must-sequence.295 (:tag :sacla)
 (assert-true (eql (reduce #'* '(1 2 3 4 5)) 120)))
(define-test sacla-must-sequence.296 (:tag :sacla)
 (assert-true
  (equal (reduce #'append '((1) (2)) :initial-value '(i n i t))
         '(i n i t 1 2))))
(define-test sacla-must-sequence.297 (:tag :sacla)
 (assert-true
  (equal (reduce #'append '((1) (2)) :from-end t :initial-value '(i n i t))
         '(1 2 i n i t))))
(define-test sacla-must-sequence.298 (:tag :sacla)
 (assert-true (eql (reduce #'- '(1 2 3 4)) -8)))
(define-test sacla-must-sequence.299 (:tag :sacla)
 (assert-true (eql (reduce #'- '(1 2 3 4) :from-end t) -2)))
(define-test sacla-must-sequence.300 (:tag :sacla)
 (assert-true (eql (reduce #'+ 'nil) 0)))
(define-test sacla-must-sequence.301 (:tag :sacla)
 (assert-true (eql (reduce #'+ '(3)) 3)))
(define-test sacla-must-sequence.302 (:tag :sacla)
 (assert-true (eq (reduce #'+ '(foo)) 'foo)))
(define-test sacla-must-sequence.303 (:tag :sacla)
 (assert-true (equal (reduce #'list '(1 2 3 4)) '(((1 2) 3) 4))))
(define-test sacla-must-sequence.304 (:tag :sacla)
 (assert-true (equal (reduce #'list '(1 2 3 4) :from-end t) '(1 (2 (3 4))))))
(define-test sacla-must-sequence.305 (:tag :sacla)
 (assert-true
  (equal (reduce #'list '(1 2 3 4) :initial-value 'foo) '((((foo 1) 2) 3) 4))))
(define-test sacla-must-sequence.306 (:tag :sacla)
 (assert-true
  (equal (reduce #'list '(1 2 3 4) :from-end t :initial-value 'foo)
         '(1 (2 (3 (4 foo)))))))
(define-test sacla-must-sequence.307 (:tag :sacla)
 (assert-true (equal (reduce #'list '(0 1 2 3)) '(((0 1) 2) 3))))
(define-test sacla-must-sequence.308 (:tag :sacla)
 (assert-true (equal (reduce #'list '(0 1 2 3) :start 1) '((1 2) 3))))
(define-test sacla-must-sequence.309 (:tag :sacla)
 (assert-true (equal (reduce #'list '(0 1 2 3) :start 1 :end nil) '((1 2) 3))))
(define-test sacla-must-sequence.310 (:tag :sacla)
 (assert-true (equal (reduce #'list '(0 1 2 3) :start 2) '(2 3))))
(define-test sacla-must-sequence.311 (:tag :sacla)
 (assert-true (eq (reduce #'list '(0 1 2 3) :start 0 :end 0) 'nil)))
(define-test sacla-must-sequence.312 (:tag :sacla)
 (assert-true
  (eq (reduce #'list '(0 1 2 3) :start 0 :end 0 :initial-value 'initial-value)
      'initial-value)))
(define-test sacla-must-sequence.313 (:tag :sacla)
 (assert-true (eq (reduce #'list '(0 1 2 3) :start 2 :end 2) 'nil)))
(define-test sacla-must-sequence.314 (:tag :sacla)
 (assert-true
  (eq (reduce #'list '(0 1 2 3) :start 2 :end 2 :initial-value 'initial-value)
      'initial-value)))
(define-test sacla-must-sequence.315 (:tag :sacla)
 (assert-true (eq (reduce #'list '(0 1 2 3) :start 4 :end 4) 'nil)))
(define-test sacla-must-sequence.316 (:tag :sacla)
 (assert-true
  (eq (reduce #'list '(0 1 2 3) :start 4 :end 4 :initial-value 'initial-value)
      'initial-value)))
(define-test sacla-must-sequence.317 (:tag :sacla)
 (assert-true (eql (reduce #'list '(0 1 2 3) :start 2 :end 3) 2)))
(define-test sacla-must-sequence.318 (:tag :sacla)
 (assert-true
  (equal
   (reduce #'list '(0 1 2 3) :start 2 :end 3 :initial-value 'initial-value)
   '(initial-value 2))))
(define-test sacla-must-sequence.319 (:tag :sacla)
 (assert-true (eql (reduce #'+ '(0 1 2 3 4 5 6 7 8 9)) 45)))
(define-test sacla-must-sequence.320 (:tag :sacla)
 (assert-true (eql (reduce #'- '(0 1 2 3 4 5 6 7 8 9)) -45)))
(define-test sacla-must-sequence.321 (:tag :sacla)
 (assert-true (eql (reduce #'- '(0 1 2 3 4 5 6 7 8 9) :from-end t) -5)))
(define-test sacla-must-sequence.322 (:tag :sacla)
 (assert-true
  (equal (reduce #'list '(0 1 2 3) :initial-value 'initial-value)
         '((((initial-value 0) 1) 2) 3))))
(define-test sacla-must-sequence.323 (:tag :sacla)
 (assert-true (equal (reduce #'list '(0 1 2 3) :from-end t) '(0 (1 (2 3))))))
(define-test sacla-must-sequence.324 (:tag :sacla)
 (assert-true
  (equal (reduce #'list '((1) (2) (3) (4)) :key #'car) '(((1 2) 3) 4))))
(define-test sacla-must-sequence.325 (:tag :sacla)
 (assert-true
  (equal (reduce #'list '((1) (2) (3) (4)) :key #'car :from-end nil)
         '(((1 2) 3) 4))))
(define-test sacla-must-sequence.326 (:tag :sacla)
 (assert-true
  (equal (reduce #'list '((1) (2) (3) (4)) :key #'car :initial-value 0)
         '((((0 1) 2) 3) 4))))
(define-test sacla-must-sequence.327 (:tag :sacla)
 (assert-true
  (equal (reduce #'list '((1) (2) (3) (4)) :key #'car :from-end t)
         '(1 (2 (3 4))))))
(define-test sacla-must-sequence.328 (:tag :sacla)
 (assert-true
  (equal
   (reduce #'list '((1) (2) (3) (4)) :key #'car :from-end t :initial-value 5)
   '(1 (2 (3 (4 5)))))))
(define-test sacla-must-sequence.329 (:tag :sacla)
 (assert-true (equal (reduce #'list #(0 1 2 3)) '(((0 1) 2) 3))))
(define-test sacla-must-sequence.330 (:tag :sacla)
 (assert-true (equal (reduce #'list #(0 1 2 3) :start 1) '((1 2) 3))))
(define-test sacla-must-sequence.331 (:tag :sacla)
 (assert-true (equal (reduce #'list #(0 1 2 3) :start 1 :end nil) '((1 2) 3))))
(define-test sacla-must-sequence.332 (:tag :sacla)
 (assert-true (equal (reduce #'list #(0 1 2 3) :start 2) '(2 3))))
(define-test sacla-must-sequence.333 (:tag :sacla)
 (assert-true (eq (reduce #'list #(0 1 2 3) :start 0 :end 0) 'nil)))
(define-test sacla-must-sequence.334 (:tag :sacla)
 (assert-true
  (eq (reduce #'list #(0 1 2 3) :start 0 :end 0 :initial-value 'initial-value)
      'initial-value)))
(define-test sacla-must-sequence.335 (:tag :sacla)
 (assert-true (eq (reduce #'list #(0 1 2 3) :start 2 :end 2) 'nil)))
(define-test sacla-must-sequence.336 (:tag :sacla)
 (assert-true
  (eq (reduce #'list #(0 1 2 3) :start 2 :end 2 :initial-value 'initial-value)
      'initial-value)))
(define-test sacla-must-sequence.337 (:tag :sacla)
 (assert-true (eq (reduce #'list #(0 1 2 3) :start 4 :end 4) 'nil)))
(define-test sacla-must-sequence.338 (:tag :sacla)
 (assert-true
  (eq (reduce #'list #(0 1 2 3) :start 4 :end 4 :initial-value 'initial-value)
      'initial-value)))
(define-test sacla-must-sequence.339 (:tag :sacla)
 (assert-true (eql (reduce #'list #(0 1 2 3) :start 2 :end 3) 2)))
(define-test sacla-must-sequence.340 (:tag :sacla)
 (assert-true
  (equal
   (reduce #'list #(0 1 2 3) :start 2 :end 3 :initial-value 'initial-value)
   '(initial-value 2))))
(define-test sacla-must-sequence.341 (:tag :sacla)
 (assert-true (eql (reduce #'+ #(0 1 2 3 4 5 6 7 8 9)) 45)))
(define-test sacla-must-sequence.342 (:tag :sacla)
 (assert-true (eql (reduce #'- #(0 1 2 3 4 5 6 7 8 9)) -45)))
(define-test sacla-must-sequence.343 (:tag :sacla)
 (assert-true (eql (reduce #'- #(0 1 2 3 4 5 6 7 8 9) :from-end t) -5)))
(define-test sacla-must-sequence.344 (:tag :sacla)
 (assert-true
  (equal (reduce #'list #(0 1 2 3) :initial-value 'initial-value)
         '((((initial-value 0) 1) 2) 3))))
(define-test sacla-must-sequence.345 (:tag :sacla)
 (assert-true (equal (reduce #'list #(0 1 2 3) :from-end t) '(0 (1 (2 3))))))
(define-test sacla-must-sequence.346 (:tag :sacla)
 (assert-true
  (equal (reduce #'list #((1) (2) (3) (4)) :key #'car) '(((1 2) 3) 4))))
(define-test sacla-must-sequence.347 (:tag :sacla)
 (assert-true
  (equal (reduce #'list #((1) (2) (3) (4)) :key #'car :from-end nil)
         '(((1 2) 3) 4))))
(define-test sacla-must-sequence.348 (:tag :sacla)
 (assert-true
  (equal (reduce #'list #((1) (2) (3) (4)) :key #'car :initial-value 0)
         '((((0 1) 2) 3) 4))))
(define-test sacla-must-sequence.349 (:tag :sacla)
 (assert-true
  (equal (reduce #'list #((1) (2) (3) (4)) :key #'car :from-end t)
         '(1 (2 (3 4))))))
(define-test sacla-must-sequence.350 (:tag :sacla)
 (assert-true
  (equal
   (reduce #'list #((1) (2) (3) (4)) :key #'car :from-end t :initial-value 5)
   '(1 (2 (3 (4 5)))))))
(define-test sacla-must-sequence.351 (:tag :sacla)
 (assert-true
  (string=
   (reduce
    #'(lambda (&rest rest)
        (concatenate 'string
                     (string (car rest))
                     (string (char-upcase (cadr rest)))))
    "abcdefg"
    :initial-value #\Z)
   "ZABCDEFG")))
(define-test sacla-must-sequence.352 (:tag :sacla)
 (assert-true (eql (count #\a "how many A's are there in here?") 2)))
(define-test sacla-must-sequence.353 (:tag :sacla)
 (assert-true (eql (count-if-not #'oddp '((1) (2) (3) (4)) :key #'car) 2)))
(define-test sacla-must-sequence.354 (:tag :sacla)
 (assert-true
  (eql (count-if #'upper-case-p "The Crying of Lot 49" :start 4) 2)))
(define-test sacla-must-sequence.355 (:tag :sacla)
 (assert-true
  (eql (count #\a (concatenate 'list "how many A's are there in here?")) 2)))
(define-test sacla-must-sequence.356 (:tag :sacla)
 (assert-true (eql (count-if #'alpha-char-p "-a-b-c-0-1-2-3-4-") 3)))
(define-test sacla-must-sequence.357 (:tag :sacla)
 (assert-true (eql (count-if #'alphanumericp "-a-b-c-0-1-2-3-4-") 8)))
(define-test sacla-must-sequence.358 (:tag :sacla)
 (assert-true (eql (count 'nil '(t nil t nil t nil)) 3)))
(define-test sacla-must-sequence.359 (:tag :sacla)
 (assert-true (eql (count 'nil #(t nil t nil t nil)) 3)))
(define-test sacla-must-sequence.360 (:tag :sacla)
 (assert-true (zerop (count 9 '(0 1 2 3 4)))))
(define-test sacla-must-sequence.361 (:tag :sacla)
 (assert-true (zerop (count 'a '(0 1 2 3 4)))))
(define-test sacla-must-sequence.362 (:tag :sacla)
 (assert-true (eql (count 0 '(0 0 0 0 0) :start 1) 4)))
(define-test sacla-must-sequence.363 (:tag :sacla)
 (assert-true (eql (count 0 '(0 0 0 0 0) :start 1 :end nil) 4)))
(define-test sacla-must-sequence.364 (:tag :sacla)
 (assert-true (eql (count 0 '(0 0 0 0 0) :start 2) 3)))
(define-test sacla-must-sequence.365 (:tag :sacla)
 (assert-true (zerop (count 0 '(0 0 0 0) :start 0 :end 0))))
(define-test sacla-must-sequence.366 (:tag :sacla)
 (assert-true (zerop (count 0 '(0 0 0 0) :start 2 :end 2))))
(define-test sacla-must-sequence.367 (:tag :sacla)
 (assert-true (zerop (count 0 '(0 0 0 0) :start 4 :end 4))))
(define-test sacla-must-sequence.368 (:tag :sacla)
 (assert-true (eql (count 0 '(0 0 0 0) :start 2 :end 3) 1)))
(define-test sacla-must-sequence.369 (:tag :sacla)
 (assert-true (eql (count #\a "abcABC" :test #'equalp) 2)))
(define-test sacla-must-sequence.370 (:tag :sacla)
 (assert-true (eql (count #\a "abcABC" :test #'char-equal) 2)))
(define-test sacla-must-sequence.371 (:tag :sacla)
 (assert-true (eql (count '(a) '((x) (y) (z) (a) (b) (c)) :test #'equalp) 1)))
(define-test sacla-must-sequence.372 (:tag :sacla)
 (assert-true (eql (count #\a "abcABC" :test-not (complement #'equalp)) 2)))
(define-test sacla-must-sequence.373 (:tag :sacla)
 (assert-true (eql (count #\a "abcABC" :test-not (complement #'char-equal)) 2)))
(define-test sacla-must-sequence.374 (:tag :sacla)
 (assert-true
  (eql (count '(a) '((x) (y) (z) (a) (b) (c)) :test-not (complement #'equalp))
       1)))
(define-test sacla-must-sequence.375 (:tag :sacla)
 (assert-true
  (eql (count 'a '((x) (y) (z) (a) (b) (c)) :key #'car :test #'eq) 1)))
(define-test sacla-must-sequence.376 (:tag :sacla)
 (assert-true
  (eql
   (count 'nil '((x . x) (y) (z . z) (a) (b . b) (c)) :key #'cdr :test #'eq)
   3)))
(define-test sacla-must-sequence.377 (:tag :sacla)
 (assert-true
  (let ((list nil))
    (and
     (eql
      (count 'a
             '(a b c d)
             :test #'(lambda (a b) (setq list (cons b list)) (eq a b)))
      1)
     (equal list '(d c b a))))))
(define-test sacla-must-sequence.378 (:tag :sacla)
 (assert-true
  (let ((list nil))
    (and
     (eql
      (count 'a
             '(a b c d)
             :test #'(lambda (a b) (setq list (cons b list)) (eq a b))
             :from-end t)
      1)
     (equal list '(a b c d))))))
(define-test sacla-must-sequence.379 (:tag :sacla)
 (assert-true (zerop (count 9 #(0 1 2 3 4)))))
(define-test sacla-must-sequence.380 (:tag :sacla)
 (assert-true (zerop (count 'a #(0 1 2 3 4)))))
(define-test sacla-must-sequence.381 (:tag :sacla)
 (assert-true (eql (count 0 #(0 0 0 0 0) :start 1) 4)))
(define-test sacla-must-sequence.382 (:tag :sacla)
 (assert-true (eql (count 0 #(0 0 0 0 0) :start 1 :end nil) 4)))
(define-test sacla-must-sequence.383 (:tag :sacla)
 (assert-true (eql (count 0 #(0 0 0 0 0) :start 2) 3)))
(define-test sacla-must-sequence.384 (:tag :sacla)
 (assert-true (zerop (count 0 #(0 0 0 0) :start 0 :end 0))))
(define-test sacla-must-sequence.385 (:tag :sacla)
 (assert-true (zerop (count 0 #(0 0 0 0) :start 2 :end 2))))
(define-test sacla-must-sequence.386 (:tag :sacla)
 (assert-true (zerop (count 0 #(0 0 0 0) :start 4 :end 4))))
(define-test sacla-must-sequence.387 (:tag :sacla)
 (assert-true (eql (count 0 #(0 0 0 0) :start 2 :end 3) 1)))
(define-test sacla-must-sequence.388 (:tag :sacla)
 (assert-true (eql (count '(a) #((x) (y) (z) (a) (b) (c)) :test #'equalp) 1)))
(define-test sacla-must-sequence.389 (:tag :sacla)
 (assert-true
  (eql (count '(a) #((x) (y) (z) (a) (b) (c)) :test-not (complement #'equalp))
       1)))
(define-test sacla-must-sequence.390 (:tag :sacla)
 (assert-true
  (eql (count 'a #((x) (y) (z) (a) (b) (c)) :key #'car :test #'eq) 1)))
(define-test sacla-must-sequence.391 (:tag :sacla)
 (assert-true
  (eql
   (count 'nil #((x . x) (y) (z . z) (a) (b . b) (c)) :key #'cdr :test #'eq)
   3)))
(define-test sacla-must-sequence.392 (:tag :sacla)
 (assert-true
  (let ((list nil))
    (and
     (eql
      (count 'a
             #(a b c d)
             :test #'(lambda (a b) (setq list (cons b list)) (eq a b)))
      1)
     (equal list '(d c b a))))))
(define-test sacla-must-sequence.393 (:tag :sacla)
 (assert-true
  (let ((list nil))
    (and
     (eql
      (count 'a
             #(a b c d)
             :test #'(lambda (a b) (setq list (cons b list)) (eq a b))
             :from-end t)
      1)
     (equal list '(a b c d))))))
(define-test sacla-must-sequence.394 (:tag :sacla)
 (assert-true (eql (count-if #'null '(t nil t nil t nil)) 3)))
(define-test sacla-must-sequence.395 (:tag :sacla)
 (assert-true (zerop (count-if #'(lambda (x) (eql x 9)) #(0 1 2 3 4)))))
(define-test sacla-must-sequence.396 (:tag :sacla)
 (assert-true (zerop (count-if #'(lambda (a) (eq 'x a)) #(0 1 2 3 4)))))
(define-test sacla-must-sequence.397 (:tag :sacla)
 (assert-true (eql (count-if #'zerop '(0 0 0 0 0) :start 1) 4)))
(define-test sacla-must-sequence.398 (:tag :sacla)
 (assert-true (eql (count-if #'zerop '(0 0 0 0 0) :start 1 :end nil) 4)))
(define-test sacla-must-sequence.399 (:tag :sacla)
 (assert-true (eql (count-if #'zerop '(0 0 0 0 0) :start 2) 3)))
(define-test sacla-must-sequence.400 (:tag :sacla)
 (assert-true (zerop (count-if #'zerop '(0 0 0 0) :start 0 :end 0))))
(define-test sacla-must-sequence.401 (:tag :sacla)
 (assert-true (zerop (count-if #'zerop '(0 0 0 0) :start 2 :end 2))))
(define-test sacla-must-sequence.402 (:tag :sacla)
 (assert-true (zerop (count-if #'zerop '(0 0 0 0) :start 4 :end 4))))
(define-test sacla-must-sequence.403 (:tag :sacla)
 (assert-true (eql (count-if #'zerop '(0 0 0 0) :start 2 :end 3) 1)))
(define-test sacla-must-sequence.404 (:tag :sacla)
 (assert-true (eql (count-if #'(lambda (x) (equalp #\a x)) "abcABC") 2)))
(define-test sacla-must-sequence.405 (:tag :sacla)
 (assert-true (eql (count-if #'(lambda (x) (char-equal #\a x)) "abcABC") 2)))
(define-test sacla-must-sequence.406 (:tag :sacla)
 (assert-true
  (eql (count-if #'(lambda (x) (equal x '(a))) '((x) (y) (z) (a) (b) (c))) 1)))
(define-test sacla-must-sequence.407 (:tag :sacla)
 (assert-true
  (eql
   (count-if #'(lambda (x) (eq x 'a)) '((x) (y) (z) (a) (b) (c)) :key #'car)
   1)))
(define-test sacla-must-sequence.408 (:tag :sacla)
 (assert-true
  (eql (count-if 'null '((x . x) (y) (z . z) (a) (b . b) (c)) :key #'cdr) 3)))
(define-test sacla-must-sequence.409 (:tag :sacla)
 (assert-true
  (eql (count-if #'(lambda (x) (equal x '(a))) '((x) (y) (z) (a) (b) (c))) 1)))
(define-test sacla-must-sequence.410 (:tag :sacla)
 (assert-true
  (eql
   (count-if #'(lambda (x) (eq x 'a)) '((x) (y) (z) (a) (b) (c)) :key #'car)
   1)))
(define-test sacla-must-sequence.411 (:tag :sacla)
 (assert-true
  (eql (count-if #'null '((x . x) (y) (z . z) (a) (b . b) (c)) :key #'cdr) 3)))
(define-test sacla-must-sequence.412 (:tag :sacla)
 (assert-true
  (let ((list nil))
    (and
     (eql
      (count-if #'(lambda (x) (setq list (cons x list)) (eq x 'a)) '(a b c d))
      1)
     (equal list '(d c b a))))))
(define-test sacla-must-sequence.413 (:tag :sacla)
 (assert-true
  (let ((list nil))
    (and
     (eql
      (count-if #'(lambda (x) (setq list (cons x list)) (eq x 'a))
                '(a b c d)
                :from-end t)
      1)
     (equal list '(a b c d))))))
(define-test sacla-must-sequence.414 (:tag :sacla)
 (assert-true (eql (count-if #'null #(t nil t nil t nil)) 3)))
(define-test sacla-must-sequence.415 (:tag :sacla)
 (assert-true (eql (count-if #'zerop #(0 0 0 0 0) :start 1) 4)))
(define-test sacla-must-sequence.416 (:tag :sacla)
 (assert-true (eql (count-if #'zerop #(0 0 0 0 0) :start 1 :end nil) 4)))
(define-test sacla-must-sequence.417 (:tag :sacla)
 (assert-true (eql (count-if #'zerop #(0 0 0 0 0) :start 2) 3)))
(define-test sacla-must-sequence.418 (:tag :sacla)
 (assert-true (zerop (count-if #'zerop #(0 0 0 0) :start 0 :end 0))))
(define-test sacla-must-sequence.419 (:tag :sacla)
 (assert-true (zerop (count-if #'zerop #(0 0 0 0) :start 2 :end 2))))
(define-test sacla-must-sequence.420 (:tag :sacla)
 (assert-true (zerop (count-if #'zerop #(0 0 0 0) :start 4 :end 4))))
(define-test sacla-must-sequence.421 (:tag :sacla)
 (assert-true (eql (count-if #'zerop #(0 0 0 0) :start 2 :end 3) 1)))
(define-test sacla-must-sequence.422 (:tag :sacla)
 (assert-true
  (eql (count-if #'(lambda (x) (equal x '(a))) #((x) (y) (z) (a) (b) (c))) 1)))
(define-test sacla-must-sequence.423 (:tag :sacla)
 (assert-true
  (eql
   (count-if #'(lambda (x) (eq x 'a)) #((x) (y) (z) (a) (b) (c)) :key #'car)
   1)))
(define-test sacla-must-sequence.424 (:tag :sacla)
 (assert-true
  (eql (count-if #'null #((x . x) (y) (z . z) (a) (b . b) (c)) :key #'cdr) 3)))
(define-test sacla-must-sequence.425 (:tag :sacla)
 (assert-true
  (eql (count-if #'(lambda (x) (equal x '(a))) #((x) (y) (z) (a) (b) (c))) 1)))
(define-test sacla-must-sequence.426 (:tag :sacla)
 (assert-true
  (eql
   (count-if #'(lambda (x) (eq x 'a)) #((x) (y) (z) (a) (b) (c)) :key #'car)
   1)))
(define-test sacla-must-sequence.427 (:tag :sacla)
 (assert-true
  (eql (count-if #'null #((x . x) (y) (z . z) (a) (b . b) (c)) :key #'cdr) 3)))
(define-test sacla-must-sequence.428 (:tag :sacla)
 (assert-true
  (let ((list nil))
    (and
     (eql
      (count-if #'(lambda (x) (setq list (cons x list)) (eq x 'a)) #(a b c d))
      1)
     (equal list '(d c b a))))))
(define-test sacla-must-sequence.429 (:tag :sacla)
 (assert-true
  (let ((list nil))
    (and
     (eql
      (count-if #'(lambda (x) (setq list (cons x list)) (eq x 'a))
                #(a b c d)
                :from-end t)
      1)
     (equal list '(a b c d))))))
(define-test sacla-must-sequence.430 (:tag :sacla)
 (assert-true (eql (count-if-not (complement #'null) '(t nil t nil t nil)) 3)))
(define-test sacla-must-sequence.431 (:tag :sacla)
 (assert-true
  (zerop (count-if-not #'(lambda (x) (not (eql x 9))) #(0 1 2 3 4)))))
(define-test sacla-must-sequence.432 (:tag :sacla)
 (assert-true
  (zerop (count-if-not #'(lambda (a) (not (eq 'x a))) #(0 1 2 3 4)))))
(define-test sacla-must-sequence.433 (:tag :sacla)
 (assert-true
  (eql (count-if-not (complement #'zerop) '(0 0 0 0 0) :start 1) 4)))
(define-test sacla-must-sequence.434 (:tag :sacla)
 (assert-true
  (eql (count-if-not (complement #'zerop) '(0 0 0 0 0) :start 1 :end nil) 4)))
(define-test sacla-must-sequence.435 (:tag :sacla)
 (assert-true
  (eql (count-if-not (complement #'zerop) '(0 0 0 0 0) :start 2) 3)))
(define-test sacla-must-sequence.436 (:tag :sacla)
 (assert-true
  (zerop (count-if-not (complement #'zerop) '(0 0 0 0) :start 0 :end 0))))
(define-test sacla-must-sequence.437 (:tag :sacla)
 (assert-true
  (zerop (count-if-not (complement #'zerop) '(0 0 0 0) :start 2 :end 2))))
(define-test sacla-must-sequence.438 (:tag :sacla)
 (assert-true
  (zerop (count-if-not (complement #'zerop) '(0 0 0 0) :start 4 :end 4))))
(define-test sacla-must-sequence.439 (:tag :sacla)
 (assert-true
  (eql (count-if-not (complement #'zerop) '(0 0 0 0) :start 2 :end 3) 1)))
(define-test sacla-must-sequence.440 (:tag :sacla)
 (assert-true
  (eql (count-if-not #'(lambda (x) (not (equalp #\a x))) "abcABC") 2)))
(define-test sacla-must-sequence.441 (:tag :sacla)
 (assert-true
  (eql (count-if-not #'(lambda (x) (not (char-equal #\a x))) "abcABC") 2)))
(define-test sacla-must-sequence.442 (:tag :sacla)
 (assert-true
  (eql
   (count-if-not #'(lambda (x) (not (equal x '(a))))
                 '((x) (y) (z) (a) (b) (c)))
   1)))
(define-test sacla-must-sequence.443 (:tag :sacla)
 (assert-true
  (eql
   (count-if-not #'(lambda (x) (not (eq x 'a)))
                 '((x) (y) (z) (a) (b) (c))
                 :key #'car)
   1)))
(define-test sacla-must-sequence.444 (:tag :sacla)
 (assert-true
  (eql
   (count-if-not (complement #'null)
                 '((x . x) (y) (z . z) (a) (b . b) (c))
                 :key #'cdr)
   3)))
(define-test sacla-must-sequence.445 (:tag :sacla)
 (assert-true
  (eql
   (count-if-not #'(lambda (x) (not (equal x '(a))))
                 '((x) (y) (z) (a) (b) (c)))
   1)))
(define-test sacla-must-sequence.446 (:tag :sacla)
 (assert-true
  (eql
   (count-if-not #'(lambda (x) (not (eq x 'a)))
                 '((x) (y) (z) (a) (b) (c))
                 :key #'car)
   1)))
(define-test sacla-must-sequence.447 (:tag :sacla)
 (assert-true
  (eql
   (count-if-not (complement #'null)
                 '((x . x) (y) (z . z) (a) (b . b) (c))
                 :key #'cdr)
   3)))
(define-test sacla-must-sequence.448 (:tag :sacla)
 (assert-true
  (let ((list nil))
    (and
     (eql
      (count-if-not #'(lambda (x) (setq list (cons x list)) (not (eq x 'a)))
                    '(a b c d))
      1)
     (equal list '(d c b a))))))
(define-test sacla-must-sequence.449 (:tag :sacla)
 (assert-true
  (let ((list nil))
    (and
     (eql
      (count-if-not #'(lambda (x) (setq list (cons x list)) (not (eq x 'a)))
                    '(a b c d)
                    :from-end t)
      1)
     (equal list '(a b c d))))))
(define-test sacla-must-sequence.450 (:tag :sacla)
 (assert-true (eql (count-if-not (complement #'null) #(t nil t nil t nil)) 3)))
(define-test sacla-must-sequence.451 (:tag :sacla)
 (assert-true
  (eql (count-if-not (complement #'zerop) #(0 0 0 0 0) :start 1) 4)))
(define-test sacla-must-sequence.452 (:tag :sacla)
 (assert-true
  (eql (count-if-not (complement #'zerop) #(0 0 0 0 0) :start 1 :end nil) 4)))
(define-test sacla-must-sequence.453 (:tag :sacla)
 (assert-true
  (eql (count-if-not (complement #'zerop) #(0 0 0 0 0) :start 2) 3)))
(define-test sacla-must-sequence.454 (:tag :sacla)
 (assert-true
  (zerop (count-if-not (complement #'zerop) #(0 0 0 0) :start 0 :end 0))))
(define-test sacla-must-sequence.455 (:tag :sacla)
 (assert-true
  (zerop (count-if-not (complement #'zerop) #(0 0 0 0) :start 2 :end 2))))
(define-test sacla-must-sequence.456 (:tag :sacla)
 (assert-true
  (zerop (count-if-not (complement #'zerop) #(0 0 0 0) :start 4 :end 4))))
(define-test sacla-must-sequence.457 (:tag :sacla)
 (assert-true
  (eql (count-if-not (complement #'zerop) #(0 0 0 0) :start 2 :end 3) 1)))
(define-test sacla-must-sequence.458 (:tag :sacla)
 (assert-true
  (eql
   (count-if-not #'(lambda (x) (not (equal x '(a))))
                 #((x) (y) (z) (a) (b) (c)))
   1)))
(define-test sacla-must-sequence.459 (:tag :sacla)
 (assert-true
  (eql
   (count-if-not #'(lambda (x) (not (eq x 'a)))
                 #((x) (y) (z) (a) (b) (c))
                 :key #'car)
   1)))
(define-test sacla-must-sequence.460 (:tag :sacla)
 (assert-true
  (eql
   (count-if-not (complement #'null)
                 #((x . x) (y) (z . z) (a) (b . b) (c))
                 :key #'cdr)
   3)))
(define-test sacla-must-sequence.461 (:tag :sacla)
 (assert-true
  (eql
   (count-if-not #'(lambda (x) (not (equal x '(a))))
                 #((x) (y) (z) (a) (b) (c)))
   1)))
(define-test sacla-must-sequence.462 (:tag :sacla)
 (assert-true
  (eql
   (count-if-not #'(lambda (x) (not (eq x 'a)))
                 #((x) (y) (z) (a) (b) (c))
                 :key #'car)
   1)))
(define-test sacla-must-sequence.463 (:tag :sacla)
 (assert-true
  (eql
   (count-if-not (complement #'null)
                 #((x . x) (y) (z . z) (a) (b . b) (c))
                 :key #'cdr)
   3)))
(define-test sacla-must-sequence.464 (:tag :sacla)
 (assert-true
  (let ((list nil))
    (and
     (eql
      (count-if-not #'(lambda (x) (setq list (cons x list)) (not (eq x 'a)))
                    #(a b c d))
      1)
     (equal list '(d c b a))))))
(define-test sacla-must-sequence.465 (:tag :sacla)
 (assert-true
  (let ((list nil))
    (and
     (eql
      (count-if-not #'(lambda (x) (setq list (cons x list)) (not (eq x 'a)))
                    #(a b c d)
                    :from-end t)
      1)
     (equal list '(a b c d))))))
(define-test sacla-must-sequence.466 (:tag :sacla)
 (assert-true (null (reverse nil))))
(define-test sacla-must-sequence.467 (:tag :sacla)
 (assert-true (string= (reverse "") "")))
(define-test sacla-must-sequence.468 (:tag :sacla)
 (assert-true (equalp (reverse #*) #*)))
(define-test sacla-must-sequence.469 (:tag :sacla)
 (assert-true (equalp (reverse #()) #())))
(define-test sacla-must-sequence.470 (:tag :sacla)
 (assert-true (equal (reverse '(0 1 2 3)) '(3 2 1 0))))
(define-test sacla-must-sequence.471 (:tag :sacla)
 (assert-true (string= (reverse "0123") "3210")))
(define-test sacla-must-sequence.472 (:tag :sacla)
 (assert-true (equalp (reverse #*1100) #*0011)))
(define-test sacla-must-sequence.473 (:tag :sacla)
 (assert-true (equalp (reverse #(a b c d)) #(d c b a))))
(define-test sacla-must-sequence.474 (:tag :sacla)
 (assert-true (null (nreverse nil))))
(define-test sacla-must-sequence.475 (:tag :sacla)
 (assert-true (string= (nreverse (copy-seq "")) "")))
(define-test sacla-must-sequence.476 (:tag :sacla)
 (assert-true (equalp (nreverse (copy-seq #*)) #*)))
(define-test sacla-must-sequence.477 (:tag :sacla)
 (assert-true (equalp (nreverse (copy-seq #())) #())))
(define-test sacla-must-sequence.478 (:tag :sacla)
 (assert-true (equal (nreverse (list 0 1 2 3)) '(3 2 1 0))))
(define-test sacla-must-sequence.479 (:tag :sacla)
 (assert-true (string= (nreverse (copy-seq "0123")) "3210")))
(define-test sacla-must-sequence.480 (:tag :sacla)
 (assert-true (equalp (reverse (copy-seq #*1100)) #*0011)))
(define-test sacla-must-sequence.481 (:tag :sacla)
 (assert-true (equalp (reverse (copy-seq #(a b c d))) #(d c b a))))
(define-test sacla-must-sequence.482 (:tag :sacla)
 (assert-true (char= (find #\d "edcba" :test #'char>) #\c)))
(define-test sacla-must-sequence.483 (:tag :sacla)
 (assert-true (eql (find-if #'oddp '(1 2 3 4 5) :end 3 :from-end t) 3)))
(define-test sacla-must-sequence.484 (:tag :sacla)
 (assert-true
  (null (find-if-not #'complexp '#(3.5 2 #C(1.0 0.0) #C(0.0 1.0)) :start 2))))
(define-test sacla-must-sequence.485 (:tag :sacla)
 (assert-true (eq (find 'a '(a b c)) 'a)))
(define-test sacla-must-sequence.486 (:tag :sacla)
 (assert-true (eq (find 'b '(a b c)) 'b)))
(define-test sacla-must-sequence.487 (:tag :sacla)
 (assert-true (eq (find 'c '(a b c)) 'c)))
(define-test sacla-must-sequence.488 (:tag :sacla)
 (assert-true (null (find 'x '(a b c)))))
(define-test sacla-must-sequence.489 (:tag :sacla)
 (assert-true (null (find 'a '(a b c) :start 1))))
(define-test sacla-must-sequence.490 (:tag :sacla)
 (assert-true (null (find 'b '(a b c) :start 2))))
(define-test sacla-must-sequence.491 (:tag :sacla)
 (assert-true (null (find 'c '(a b c) :start 3))))
(define-test sacla-must-sequence.492 (:tag :sacla)
 (assert-true (null (find 'a '(a b c) :start 0 :end 0))))
(define-test sacla-must-sequence.493 (:tag :sacla)
 (assert-true (null (find 'a '(a b c) :start 0 :end 0 :from-end t))))
(define-test sacla-must-sequence.494 (:tag :sacla)
 (assert-true (null (find 'a '(a b c) :start 1 :end 1))))
(define-test sacla-must-sequence.495 (:tag :sacla)
 (assert-true (null (find 'a '(a b c) :start 1 :end 1 :from-end t))))
(define-test sacla-must-sequence.496 (:tag :sacla)
 (assert-true (null (find 'a '(a b c) :start 2 :end 2))))
(define-test sacla-must-sequence.497 (:tag :sacla)
 (assert-true (null (find 'a '(a b c) :start 2 :end 2 :from-end t))))
(define-test sacla-must-sequence.498 (:tag :sacla)
 (assert-true (null (find 'a '(a b c) :start 3 :end 3))))
(define-test sacla-must-sequence.499 (:tag :sacla)
 (assert-true (null (find 'a '(a b c) :start 3 :end 3 :from-end t))))
(define-test sacla-must-sequence.500 (:tag :sacla)
 (assert-true (eq (find 'a '(a b c) :end nil) 'a)))
(define-test sacla-must-sequence.501 (:tag :sacla)
 (assert-true (eq (find 'b '(a b c) :end nil) 'b)))
(define-test sacla-must-sequence.502 (:tag :sacla)
 (assert-true (eq (find 'c '(a b c) :end nil) 'c)))
(define-test sacla-must-sequence.503 (:tag :sacla)
 (assert-true (eq (find 'a '(a b c) :end 1) 'a)))
(define-test sacla-must-sequence.504 (:tag :sacla)
 (assert-true (eq (find 'b '(a b c) :end 2) 'b)))
(define-test sacla-must-sequence.505 (:tag :sacla)
 (assert-true (eq (find 'c '(a b c) :end 3) 'c)))
(define-test sacla-must-sequence.506 (:tag :sacla)
 (assert-true (null (find 'a '(a b c) :end 0))))
(define-test sacla-must-sequence.507 (:tag :sacla)
 (assert-true (null (find 'b '(a b c) :end 1))))
(define-test sacla-must-sequence.508 (:tag :sacla)
 (assert-true (null (find 'c '(a b c) :end 2))))
(define-test sacla-must-sequence.509 (:tag :sacla)
 (assert-true (null (find 'a '((a) (b) (c))))))
(define-test sacla-must-sequence.510 (:tag :sacla)
 (assert-true (equal (find 'a '((a) (b) (c)) :key #'car) '(a))))
(define-test sacla-must-sequence.511 (:tag :sacla)
 (assert-true (equal (find 'b '((a) (b) (c)) :key #'car) '(b))))
(define-test sacla-must-sequence.512 (:tag :sacla)
 (assert-true (equal (find 'c '((a) (b) (c)) :key #'car) '(c))))
(define-test sacla-must-sequence.513 (:tag :sacla)
 (assert-true (null (find 'z '((a) (b) (c)) :key #'car))))
(define-test sacla-must-sequence.514 (:tag :sacla)
 (assert-true
  (let ((list '((a) (b) (c))))
    (and (eq (find 'a list :key #'car) (car list))
         (eq (find 'b list :key #'car) (cadr list))
         (eq (find 'c list :key #'car) (caddr list))
         (null (find 'z list :key #'car))))))
(define-test sacla-must-sequence.515 (:tag :sacla)
 (assert-true (null (find '(a) '((a) (b) (c))))))
(define-test sacla-must-sequence.516 (:tag :sacla)
 (assert-true (equal (find '(a) '((a) (b) (c)) :test #'equal) '(a))))
(define-test sacla-must-sequence.517 (:tag :sacla)
 (assert-true (null (find '("a") '(("a") ("b") ("c"))))))
(define-test sacla-must-sequence.518 (:tag :sacla)
 (assert-true (null (find '("a") '(("A") ("B") ("c")) :test #'equal))))
(define-test sacla-must-sequence.519 (:tag :sacla)
 (assert-true (equal (find '("a") '(("A") ("B") ("c")) :test #'equalp) '("A"))))
(define-test sacla-must-sequence.520 (:tag :sacla)
 (assert-true
  (eq (find 'nil '(first second third) :test (constantly t)) 'first)))
(define-test sacla-must-sequence.521 (:tag :sacla)
 (assert-true (eql (find 3 '(0 1 2 3 4 5)) 3)))
(define-test sacla-must-sequence.522 (:tag :sacla)
 (assert-true (eql (find 3 '(0 1 2 3 4 5) :test #'<) 4)))
(define-test sacla-must-sequence.523 (:tag :sacla)
 (assert-true (eql (find 3 '(0 1 2 3 4 5) :test #'>) 0)))
(define-test sacla-must-sequence.524 (:tag :sacla)
 (assert-true
  (equal (find '(a) '((a) (b) (c)) :test-not (complement #'equal)) '(a))))
(define-test sacla-must-sequence.525 (:tag :sacla)
 (assert-true
  (null (find '("a") '(("A") ("B") ("c")) :test-not (complement #'equal)))))
(define-test sacla-must-sequence.526 (:tag :sacla)
 (assert-true
  (equal (find '("a") '(("A") ("B") ("c")) :test-not (complement #'equalp))
         '("A"))))
(define-test sacla-must-sequence.527 (:tag :sacla)
 (assert-true
  (eq (find 'nil '(first second third) :test-not (constantly nil)) 'first)))
(define-test sacla-must-sequence.528 (:tag :sacla)
 (assert-true (eql (find 3 '(0 1 2 3 4 5) :test-not #'>=) 4)))
(define-test sacla-must-sequence.529 (:tag :sacla)
 (assert-true (eql (find 3 '(0 1 2 3 4 5) :test-not #'<=) 0)))
(define-test sacla-must-sequence.530 (:tag :sacla)
 (assert-true
  (equal (find 'a '((a) (b) (c) (a a) (b b) (c c)) :key #'car) '(a))))
(define-test sacla-must-sequence.531 (:tag :sacla)
 (assert-true
  (equal (find 'a '((a) (b) (c) (a a) (b b) (c c)) :key #'car :from-end t)
         '(a a))))
(define-test sacla-must-sequence.532 (:tag :sacla)
 (assert-true
  (equal (find 'b '((a) (b) (c) (a a) (b b) (c c)) :key #'car) '(b))))
(define-test sacla-must-sequence.533 (:tag :sacla)
 (assert-true
  (equal (find 'b '((a) (b) (c) (a a) (b b) (c c)) :key #'car :from-end t)
         '(b b))))
(define-test sacla-must-sequence.534 (:tag :sacla)
 (assert-true
  (equal (find 'c '((a) (b) (c) (a a) (b b) (c c)) :key #'car) '(c))))
(define-test sacla-must-sequence.535 (:tag :sacla)
 (assert-true
  (equal (find 'c '((a) (b) (c) (a a) (b b) (c c)) :key #'car :from-end t)
         '(c c))))
(define-test sacla-must-sequence.536 (:tag :sacla)
 (assert-true (null (find 'z '((a) (b) (c) (a a) (b b) (c c)) :key #'car))))
(define-test sacla-must-sequence.537 (:tag :sacla)
 (assert-true
  (null (find 'z '((a) (b) (c) (a a) (b b) (c c)) :key #'car :from-end t))))
(define-test sacla-must-sequence.538 (:tag :sacla)
 (assert-true
  (equal
   (find 'a '((a) (b) (c) (a a) (b b) (c c) (a a a)) :key #'car :from-end t)
   '(a a a))))
(define-test sacla-must-sequence.539 (:tag :sacla)
 (assert-true
  (equal
   (find 'a
         '((a) (b) (c) (a a) (b b) (c c) (a a a))
         :key #'car
         :from-end t
         :end nil)
   '(a a a))))
(define-test sacla-must-sequence.540 (:tag :sacla)
 (assert-true
  (equal
   (find 'a
         '((a) (b) (c) (a a) (b b) (c c) (a a a))
         :key #'car
         :from-end t
         :end 6)
   '(a a))))
(define-test sacla-must-sequence.541 (:tag :sacla)
 (assert-true
  (null
   (find 'a
         '((a) (b) (c) (a a) (b b) (c c) (a a a))
         :key #'car
         :from-end t
         :start 1
         :end 3))))
(define-test sacla-must-sequence.542 (:tag :sacla)
 (assert-true (null (find #\c '("abc" "bcd" "cde")))))
(define-test sacla-must-sequence.543 (:tag :sacla)
 (assert-true
  (string=
   (find #\c
         '("abc" "bcd" "cde")
         :key #'(lambda (arg) (char arg 0))
         :test #'char=)
   "cde")))
(define-test sacla-must-sequence.544 (:tag :sacla)
 (assert-true
  (string=
   (find #\c
         '("abc" "bcd" "cde")
         :key #'(lambda (arg) (char arg 0))
         :test #'char>)
   "abc")))
(define-test sacla-must-sequence.545 (:tag :sacla)
 (assert-true
  (string=
   (find #\c
         '("abc" "bcd" "cde")
         :start 1
         :key #'(lambda (arg) (char arg 0))
         :test #'char>)
   "bcd")))
(define-test sacla-must-sequence.546 (:tag :sacla)
 (assert-true (eq (find 'a #(a b c)) 'a)))
(define-test sacla-must-sequence.547 (:tag :sacla)
 (assert-true (eq (find 'b #(a b c)) 'b)))
(define-test sacla-must-sequence.548 (:tag :sacla)
 (assert-true (eq (find 'c #(a b c)) 'c)))
(define-test sacla-must-sequence.549 (:tag :sacla)
 (assert-true (null (find 'x #(a b c)))))
(define-test sacla-must-sequence.550 (:tag :sacla)
 (assert-true (null (find 'a #(a b c) :start 1))))
(define-test sacla-must-sequence.551 (:tag :sacla)
 (assert-true (null (find 'b #(a b c) :start 2))))
(define-test sacla-must-sequence.552 (:tag :sacla)
 (assert-true (null (find 'c #(a b c) :start 3))))
(define-test sacla-must-sequence.553 (:tag :sacla)
 (assert-true (null (find 'a #(a b c) :start 0 :end 0))))
(define-test sacla-must-sequence.554 (:tag :sacla)
 (assert-true (null (find 'a #(a b c) :start 0 :end 0 :from-end t))))
(define-test sacla-must-sequence.555 (:tag :sacla)
 (assert-true (null (find 'a #(a b c) :start 1 :end 1))))
(define-test sacla-must-sequence.556 (:tag :sacla)
 (assert-true (null (find 'a #(a b c) :start 1 :end 1 :from-end t))))
(define-test sacla-must-sequence.557 (:tag :sacla)
 (assert-true (null (find 'a #(a b c) :start 2 :end 2))))
(define-test sacla-must-sequence.558 (:tag :sacla)
 (assert-true (null (find 'a #(a b c) :start 2 :end 2 :from-end t))))
(define-test sacla-must-sequence.559 (:tag :sacla)
 (assert-true (null (find 'a #(a b c) :start 3 :end 3))))
(define-test sacla-must-sequence.560 (:tag :sacla)
 (assert-true (null (find 'a #(a b c) :start 3 :end 3 :from-end t))))
(define-test sacla-must-sequence.561 (:tag :sacla)
 (assert-true (eq (find 'a #(a b c) :end nil) 'a)))
(define-test sacla-must-sequence.562 (:tag :sacla)
 (assert-true (eq (find 'b #(a b c) :end nil) 'b)))
(define-test sacla-must-sequence.563 (:tag :sacla)
 (assert-true (eq (find 'c #(a b c) :end nil) 'c)))
(define-test sacla-must-sequence.564 (:tag :sacla)
 (assert-true (eq (find 'a #(a b c) :end 1) 'a)))
(define-test sacla-must-sequence.565 (:tag :sacla)
 (assert-true (eq (find 'b #(a b c) :end 2) 'b)))
(define-test sacla-must-sequence.566 (:tag :sacla)
 (assert-true (eq (find 'c #(a b c) :end 3) 'c)))
(define-test sacla-must-sequence.567 (:tag :sacla)
 (assert-true (null (find 'a #(a b c) :end 0))))
(define-test sacla-must-sequence.568 (:tag :sacla)
 (assert-true (null (find 'b #(a b c) :end 1))))
(define-test sacla-must-sequence.569 (:tag :sacla)
 (assert-true (null (find 'c #(a b c) :end 2))))
(define-test sacla-must-sequence.570 (:tag :sacla)
 (assert-true (null (find 'a #((a) (b) (c))))))
(define-test sacla-must-sequence.571 (:tag :sacla)
 (assert-true (equal (find 'a #((a) (b) (c)) :key #'car) '(a))))
(define-test sacla-must-sequence.572 (:tag :sacla)
 (assert-true (equal (find 'b #((a) (b) (c)) :key #'car) '(b))))
(define-test sacla-must-sequence.573 (:tag :sacla)
 (assert-true (equal (find 'c #((a) (b) (c)) :key #'car) '(c))))
(define-test sacla-must-sequence.574 (:tag :sacla)
 (assert-true (null (find 'z #((a) (b) (c)) :key #'car))))
(define-test sacla-must-sequence.575 (:tag :sacla)
 (assert-true
  (let ((vector #((a) (b) (c))))
    (and (eq (find 'a vector :key #'car) (aref vector 0))
         (eq (find 'b vector :key #'car) (aref vector 1))
         (eq (find 'c vector :key #'car) (aref vector 2))
         (null (find 'z vector :key #'car))))))
(define-test sacla-must-sequence.576 (:tag :sacla)
 (assert-true (null (find '(a) #((a) (b) (c))))))
(define-test sacla-must-sequence.577 (:tag :sacla)
 (assert-true (equal (find '(a) #((a) (b) (c)) :test #'equal) '(a))))
(define-test sacla-must-sequence.578 (:tag :sacla)
 (assert-true (null (find '("a") #(("a") ("b") ("c"))))))
(define-test sacla-must-sequence.579 (:tag :sacla)
 (assert-true (null (find '("a") #(("A") ("B") ("c")) :test #'equal))))
(define-test sacla-must-sequence.580 (:tag :sacla)
 (assert-true (equal (find '("a") #(("A") ("B") ("c")) :test #'equalp) '("A"))))
(define-test sacla-must-sequence.581 (:tag :sacla)
 (assert-true
  (eq (find 'nil #(first second third) :test (constantly t)) 'first)))
(define-test sacla-must-sequence.582 (:tag :sacla)
 (assert-true (eql (find 3 #(0 1 2 3 4 5)) 3)))
(define-test sacla-must-sequence.583 (:tag :sacla)
 (assert-true (eql (find 3 #(0 1 2 3 4 5) :test #'<) 4)))
(define-test sacla-must-sequence.584 (:tag :sacla)
 (assert-true (eql (find 3 #(0 1 2 3 4 5) :test #'>) 0)))
(define-test sacla-must-sequence.585 (:tag :sacla)
 (assert-true
  (equal (find '(a) #((a) (b) (c)) :test-not (complement #'equal)) '(a))))
(define-test sacla-must-sequence.586 (:tag :sacla)
 (assert-true
  (null (find '("a") #(("A") ("B") ("c")) :test-not (complement #'equal)))))
(define-test sacla-must-sequence.587 (:tag :sacla)
 (assert-true
  (equal (find '("a") #(("A") ("B") ("c")) :test-not (complement #'equalp))
         '("A"))))
(define-test sacla-must-sequence.588 (:tag :sacla)
 (assert-true
  (eq (find 'nil #(first second third) :test-not (constantly nil)) 'first)))
(define-test sacla-must-sequence.589 (:tag :sacla)
 (assert-true (eql (find 3 #(0 1 2 3 4 5) :test-not #'>=) 4)))
(define-test sacla-must-sequence.590 (:tag :sacla)
 (assert-true (eql (find 3 #(0 1 2 3 4 5) :test-not #'<=) 0)))
(define-test sacla-must-sequence.591 (:tag :sacla)
 (assert-true
  (equal (find 'a #((a) (b) (c) (a a) (b b) (c c)) :key #'car) '(a))))
(define-test sacla-must-sequence.592 (:tag :sacla)
 (assert-true
  (equal (find 'a #((a) (b) (c) (a a) (b b) (c c)) :key #'car :from-end t)
         '(a a))))
(define-test sacla-must-sequence.593 (:tag :sacla)
 (assert-true
  (equal (find 'b #((a) (b) (c) (a a) (b b) (c c)) :key #'car) '(b))))
(define-test sacla-must-sequence.594 (:tag :sacla)
 (assert-true
  (equal (find 'b #((a) (b) (c) (a a) (b b) (c c)) :key #'car :from-end t)
         '(b b))))
(define-test sacla-must-sequence.595 (:tag :sacla)
 (assert-true
  (equal (find 'c #((a) (b) (c) (a a) (b b) (c c)) :key #'car) '(c))))
(define-test sacla-must-sequence.596 (:tag :sacla)
 (assert-true
  (equal (find 'c #((a) (b) (c) (a a) (b b) (c c)) :key #'car :from-end t)
         '(c c))))
(define-test sacla-must-sequence.597 (:tag :sacla)
 (assert-true (null (find 'z #((a) (b) (c) (a a) (b b) (c c)) :key #'car))))
(define-test sacla-must-sequence.598 (:tag :sacla)
 (assert-true
  (null (find 'z #((a) (b) (c) (a a) (b b) (c c)) :key #'car :from-end t))))
(define-test sacla-must-sequence.599 (:tag :sacla)
 (assert-true
  (equal
   (find 'a #((a) (b) (c) (a a) (b b) (c c) (a a a)) :key #'car :from-end t)
   '(a a a))))
(define-test sacla-must-sequence.600 (:tag :sacla)
 (assert-true
  (equal
   (find 'a
         #((a) (b) (c) (a a) (b b) (c c) (a a a))
         :key #'car
         :from-end t
         :end nil)
   '(a a a))))
(define-test sacla-must-sequence.601 (:tag :sacla)
 (assert-true
  (equal
   (find 'a
         #((a) (b) (c) (a a) (b b) (c c) (a a a))
         :key #'car
         :from-end t
         :end 6)
   '(a a))))
(define-test sacla-must-sequence.602 (:tag :sacla)
 (assert-true
  (null
   (find 'a
         #((a) (b) (c) (a a) (b b) (c c) (a a a))
         :key #'car
         :from-end t
         :start 1
         :end 3))))
(define-test sacla-must-sequence.603 (:tag :sacla)
 (assert-true (null (find #\c #("abc" "bcd" "cde")))))
(define-test sacla-must-sequence.604 (:tag :sacla)
 (assert-true
  (string=
   (find #\c
         #("abc" "bcd" "cde")
         :key #'(lambda (arg) (char arg 0))
         :test #'char=)
   "cde")))
(define-test sacla-must-sequence.605 (:tag :sacla)
 (assert-true
  (string=
   (find #\c
         #("abc" "bcd" "cde")
         :key #'(lambda (arg) (char arg 0))
         :test #'char>)
   "abc")))
(define-test sacla-must-sequence.606 (:tag :sacla)
 (assert-true
  (string=
   (find #\c
         #("abc" "bcd" "cde")
         :start 1
         :key #'(lambda (arg) (char arg 0))
         :test #'char>)
   "bcd")))
(define-test sacla-must-sequence.607 (:tag :sacla)
 (assert-true (null (find #\z "abcABC"))))
(define-test sacla-must-sequence.608 (:tag :sacla)
 (assert-true (eql (find #\a "abcABC") #\a)))
(define-test sacla-must-sequence.609 (:tag :sacla)
 (assert-true (eql (find #\A "abcABC") #\A)))
(define-test sacla-must-sequence.610 (:tag :sacla)
 (assert-true (eql (find #\A "abcABC" :test #'char-equal) #\a)))
(define-test sacla-must-sequence.611 (:tag :sacla)
 (assert-true (eql (find #\A "abcABC" :test #'char-equal :from-end t) #\A)))
(define-test sacla-must-sequence.612 (:tag :sacla)
 (assert-true (eql (find #\a "abcABC" :test #'char-equal :from-end t) #\A)))
(define-test sacla-must-sequence.613 (:tag :sacla)
 (assert-true
  (eql (find #\a "abcABC" :test #'char-equal :from-end t :end 4) #\A)))
(define-test sacla-must-sequence.614 (:tag :sacla)
 (assert-true
  (eql (find #\a "abcABC" :test #'char-equal :from-end t :end 3) #\a)))
(define-test sacla-must-sequence.615 (:tag :sacla)
 (assert-true (zerop (find 0 #*01))))
(define-test sacla-must-sequence.616 (:tag :sacla)
 (assert-true (eql (find 1 #*01) 1)))
(define-test sacla-must-sequence.617 (:tag :sacla)
 (assert-true (null (find 0 #*01 :start 1))))
(define-test sacla-must-sequence.618 (:tag :sacla)
 (assert-true (null (find 1 #*01 :end 0))))
(define-test sacla-must-sequence.619 (:tag :sacla)
 (assert-true (null (find 0 #*000001 :start 5))))
(define-test sacla-must-sequence.620 (:tag :sacla)
 (assert-true (eq (find-if #'(lambda (x) (eq x 'a)) '(a b c)) 'a)))
(define-test sacla-must-sequence.621 (:tag :sacla)
 (assert-true (eq (find-if #'(lambda (x) (eq x 'b)) '(a b c)) 'b)))
(define-test sacla-must-sequence.622 (:tag :sacla)
 (assert-true (eq (find-if #'(lambda (x) (eq x 'c)) '(a b c)) 'c)))
(define-test sacla-must-sequence.623 (:tag :sacla)
 (assert-true (null (find-if #'(lambda (arg) (eq arg 'x)) '(a b c)))))
(define-test sacla-must-sequence.624 (:tag :sacla)
 (assert-true (null (find-if #'(lambda (x) (eq x 'a)) '(a b c) :start 1))))
(define-test sacla-must-sequence.625 (:tag :sacla)
 (assert-true (null (find-if #'(lambda (x) (eq x 'b)) '(a b c) :start 2))))
(define-test sacla-must-sequence.626 (:tag :sacla)
 (assert-true (null (find-if #'(lambda (x) (eq x 'c)) '(a b c) :start 3))))
(define-test sacla-must-sequence.627 (:tag :sacla)
 (assert-true
  (null (find-if #'(lambda (x) (eq x 'a)) '(a b c) :start 0 :end 0))))
(define-test sacla-must-sequence.628 (:tag :sacla)
 (assert-true
  (null
   (find-if #'(lambda (x) (eq x 'a)) '(a b c) :start 0 :end 0 :from-end t))))
(define-test sacla-must-sequence.629 (:tag :sacla)
 (assert-true
  (null (find-if #'(lambda (x) (eq x 'a)) '(a b c) :start 1 :end 1))))
(define-test sacla-must-sequence.630 (:tag :sacla)
 (assert-true
  (null
   (find-if #'(lambda (x) (eq x 'a)) '(a b c) :start 1 :end 1 :from-end t))))
(define-test sacla-must-sequence.631 (:tag :sacla)
 (assert-true
  (null (find-if #'(lambda (x) (eq x 'a)) '(a b c) :start 2 :end 2))))
(define-test sacla-must-sequence.632 (:tag :sacla)
 (assert-true
  (null
   (find-if #'(lambda (x) (eq x 'a)) '(a b c) :start 2 :end 2 :from-end t))))
(define-test sacla-must-sequence.633 (:tag :sacla)
 (assert-true
  (null (find-if #'(lambda (x) (eq x 'a)) '(a b c) :start 3 :end 3))))
(define-test sacla-must-sequence.634 (:tag :sacla)
 (assert-true
  (null
   (find-if #'(lambda (x) (eq x 'a)) '(a b c) :start 3 :end 3 :from-end t))))
(define-test sacla-must-sequence.635 (:tag :sacla)
 (assert-true (eq (find-if #'(lambda (x) (eq x 'a)) '(a b c) :end nil) 'a)))
(define-test sacla-must-sequence.636 (:tag :sacla)
 (assert-true (eq (find-if #'(lambda (x) (eq x 'b)) '(a b c) :end nil) 'b)))
(define-test sacla-must-sequence.637 (:tag :sacla)
 (assert-true (eq (find-if #'(lambda (x) (eq x 'c)) '(a b c) :end nil) 'c)))
(define-test sacla-must-sequence.638 (:tag :sacla)
 (assert-true (eq (find-if #'(lambda (x) (eq x 'a)) '(a b c) :end 1) 'a)))
(define-test sacla-must-sequence.639 (:tag :sacla)
 (assert-true (eq (find-if #'(lambda (x) (eq x 'b)) '(a b c) :end 2) 'b)))
(define-test sacla-must-sequence.640 (:tag :sacla)
 (assert-true (eq (find-if #'(lambda (x) (eq x 'c)) '(a b c) :end 3) 'c)))
(define-test sacla-must-sequence.641 (:tag :sacla)
 (assert-true (null (find-if #'(lambda (x) (eq x 'a)) '(a b c) :end 0))))
(define-test sacla-must-sequence.642 (:tag :sacla)
 (assert-true (null (find-if #'(lambda (x) (eq x 'b)) '(a b c) :end 1))))
(define-test sacla-must-sequence.643 (:tag :sacla)
 (assert-true (null (find-if #'(lambda (x) (eq x 'c)) '(a b c) :end 2))))
(define-test sacla-must-sequence.644 (:tag :sacla)
 (assert-true (null (find-if #'(lambda (x) (eq x 'a)) '((a) (b) (c))))))
(define-test sacla-must-sequence.645 (:tag :sacla)
 (assert-true
  (equal (find-if #'(lambda (x) (eq x 'a)) '((a) (b) (c)) :key #'car) '(a))))
(define-test sacla-must-sequence.646 (:tag :sacla)
 (assert-true
  (equal (find-if #'(lambda (x) (eq x 'b)) '((a) (b) (c)) :key #'car) '(b))))
(define-test sacla-must-sequence.647 (:tag :sacla)
 (assert-true
  (equal (find-if #'(lambda (x) (eq x 'c)) '((a) (b) (c)) :key #'car) '(c))))
(define-test sacla-must-sequence.648 (:tag :sacla)
 (assert-true
  (null (find-if #'(lambda (x) (eq x 'z)) '((a) (b) (c)) :key #'car))))
(define-test sacla-must-sequence.649 (:tag :sacla)
 (assert-true
  (let ((list '((a) (b) (c))))
    (and (eq (find-if #'(lambda (x) (eq x 'a)) list :key #'car) (car list))
         (eq (find-if #'(lambda (x) (eq x 'b)) list :key #'car) (cadr list))
         (eq (find-if #'(lambda (x) (eq x 'c)) list :key #'car) (caddr list))
         (null (find-if #'(lambda (x) (eq x 'z)) list :key #'car))))))
(define-test sacla-must-sequence.650 (:tag :sacla)
 (assert-true
  (equal
   (find-if #'(lambda (x) (eq x 'a))
            '((a) (b) (c) (a a) (b b) (c c))
            :key #'car)
   '(a))))
(define-test sacla-must-sequence.651 (:tag :sacla)
 (assert-true
  (equal
   (find-if #'(lambda (x) (eq x 'a))
            '((a) (b) (c) (a a) (b b) (c c))
            :key #'car
            :from-end t)
   '(a a))))
(define-test sacla-must-sequence.652 (:tag :sacla)
 (assert-true
  (equal
   (find-if #'(lambda (x) (eq x 'b))
            '((a) (b) (c) (a a) (b b) (c c))
            :key #'car)
   '(b))))
(define-test sacla-must-sequence.653 (:tag :sacla)
 (assert-true
  (equal
   (find-if #'(lambda (x) (eq x 'b))
            '((a) (b) (c) (a a) (b b) (c c))
            :key #'car
            :from-end t)
   '(b b))))
(define-test sacla-must-sequence.654 (:tag :sacla)
 (assert-true
  (equal
   (find-if #'(lambda (x) (eq x 'c))
            '((a) (b) (c) (a a) (b b) (c c))
            :key #'car)
   '(c))))
(define-test sacla-must-sequence.655 (:tag :sacla)
 (assert-true
  (equal
   (find-if #'(lambda (x) (eq x 'c))
            '((a) (b) (c) (a a) (b b) (c c))
            :key #'car
            :from-end t)
   '(c c))))
(define-test sacla-must-sequence.656 (:tag :sacla)
 (assert-true
  (null
   (find-if #'(lambda (x) (eq x 'z))
            '((a) (b) (c) (a a) (b b) (c c))
            :key #'car))))
(define-test sacla-must-sequence.657 (:tag :sacla)
 (assert-true
  (null
   (find-if #'(lambda (x) (eq x 'z))
            '((a) (b) (c) (a a) (b b) (c c))
            :key #'car
            :from-end t))))
(define-test sacla-must-sequence.658 (:tag :sacla)
 (assert-true
  (equal
   (find-if #'(lambda (x) (eq x 'a))
            '((a) (b) (c) (a a) (b b) (c c) (a a a))
            :key #'car
            :from-end t)
   '(a a a))))
(define-test sacla-must-sequence.659 (:tag :sacla)
 (assert-true
  (equal
   (find-if #'(lambda (x) (eq x 'a))
            '((a) (b) (c) (a a) (b b) (c c) (a a a))
            :key #'car
            :from-end t
            :end nil)
   '(a a a))))
(define-test sacla-must-sequence.660 (:tag :sacla)
 (assert-true
  (equal
   (find-if #'(lambda (x) (eq x 'a))
            '((a) (b) (c) (a a) (b b) (c c) (a a a))
            :key #'car
            :from-end t
            :end 6)
   '(a a))))
(define-test sacla-must-sequence.661 (:tag :sacla)
 (assert-true
  (null
   (find-if #'(lambda (x) (eq x 'a))
            '((a) (b) (c) (a a) (b b) (c c) (a a a))
            :key #'car
            :from-end t
            :start 1
            :end 3))))
(define-test sacla-must-sequence.662 (:tag :sacla)
 (assert-true (null (find-if #'(lambda (x) (eql x #\c)) '("abc" "bcd" "cde")))))
(define-test sacla-must-sequence.663 (:tag :sacla)
 (assert-true
  (string=
   (find-if #'(lambda (x) (eql x #\c))
            '("abc" "bcd" "cde")
            :key #'(lambda (arg) (char arg 0)))
   "cde")))
(define-test sacla-must-sequence.664 (:tag :sacla)
 (assert-true
  (string=
   (find-if #'(lambda (x) (char> #\c x))
            '("abc" "bcd" "cde")
            :key #'(lambda (arg) (char arg 0)))
   "abc")))
(define-test sacla-must-sequence.665 (:tag :sacla)
 (assert-true
  (string=
   (find-if #'(lambda (x) (char> #\c x))
            '("abc" "bcd" "cde")
            :start 1
            :key #'(lambda (arg) (char arg 0)))
   "bcd")))
(define-test sacla-must-sequence.666 (:tag :sacla)
 (assert-true (eq (find-if #'(lambda (x) (eq x 'a)) #(a b c)) 'a)))
(define-test sacla-must-sequence.667 (:tag :sacla)
 (assert-true (eq (find-if #'(lambda (x) (eq x 'b)) #(a b c)) 'b)))
(define-test sacla-must-sequence.668 (:tag :sacla)
 (assert-true (eq (find-if #'(lambda (x) (eq x 'c)) #(a b c)) 'c)))
(define-test sacla-must-sequence.669 (:tag :sacla)
 (assert-true (null (find-if #'(lambda (arg) (eq arg 'x)) #(a b c)))))
(define-test sacla-must-sequence.670 (:tag :sacla)
 (assert-true (null (find-if #'(lambda (x) (eq x 'a)) #(a b c) :start 1))))
(define-test sacla-must-sequence.671 (:tag :sacla)
 (assert-true (null (find-if #'(lambda (x) (eq x 'b)) #(a b c) :start 2))))
(define-test sacla-must-sequence.672 (:tag :sacla)
 (assert-true (null (find-if #'(lambda (x) (eq x 'c)) #(a b c) :start 3))))
(define-test sacla-must-sequence.673 (:tag :sacla)
 (assert-true
  (null (find-if #'(lambda (x) (eq x 'a)) #(a b c) :start 0 :end 0))))
(define-test sacla-must-sequence.674 (:tag :sacla)
 (assert-true
  (null
   (find-if #'(lambda (x) (eq x 'a)) #(a b c) :start 0 :end 0 :from-end t))))
(define-test sacla-must-sequence.675 (:tag :sacla)
 (assert-true
  (null (find-if #'(lambda (x) (eq x 'a)) #(a b c) :start 1 :end 1))))
(define-test sacla-must-sequence.676 (:tag :sacla)
 (assert-true
  (null
   (find-if #'(lambda (x) (eq x 'a)) #(a b c) :start 1 :end 1 :from-end t))))
(define-test sacla-must-sequence.677 (:tag :sacla)
 (assert-true
  (null (find-if #'(lambda (x) (eq x 'a)) #(a b c) :start 2 :end 2))))
(define-test sacla-must-sequence.678 (:tag :sacla)
 (assert-true
  (null
   (find-if #'(lambda (x) (eq x 'a)) #(a b c) :start 2 :end 2 :from-end t))))
(define-test sacla-must-sequence.679 (:tag :sacla)
 (assert-true
  (null (find-if #'(lambda (x) (eq x 'a)) #(a b c) :start 3 :end 3))))
(define-test sacla-must-sequence.680 (:tag :sacla)
 (assert-true
  (null
   (find-if #'(lambda (x) (eq x 'a)) #(a b c) :start 3 :end 3 :from-end t))))
(define-test sacla-must-sequence.681 (:tag :sacla)
 (assert-true (eq (find-if #'(lambda (x) (eq x 'a)) #(a b c) :end nil) 'a)))
(define-test sacla-must-sequence.682 (:tag :sacla)
 (assert-true (eq (find-if #'(lambda (x) (eq x 'b)) #(a b c) :end nil) 'b)))
(define-test sacla-must-sequence.683 (:tag :sacla)
 (assert-true (eq (find-if #'(lambda (x) (eq x 'c)) #(a b c) :end nil) 'c)))
(define-test sacla-must-sequence.684 (:tag :sacla)
 (assert-true (eq (find-if #'(lambda (x) (eq x 'a)) #(a b c) :end 1) 'a)))
(define-test sacla-must-sequence.685 (:tag :sacla)
 (assert-true (eq (find-if #'(lambda (x) (eq x 'b)) #(a b c) :end 2) 'b)))
(define-test sacla-must-sequence.686 (:tag :sacla)
 (assert-true (eq (find-if #'(lambda (x) (eq x 'c)) #(a b c) :end 3) 'c)))
(define-test sacla-must-sequence.687 (:tag :sacla)
 (assert-true (null (find-if #'(lambda (x) (eq x 'a)) #(a b c) :end 0))))
(define-test sacla-must-sequence.688 (:tag :sacla)
 (assert-true (null (find-if #'(lambda (x) (eq x 'b)) #(a b c) :end 1))))
(define-test sacla-must-sequence.689 (:tag :sacla)
 (assert-true (null (find-if #'(lambda (x) (eq x 'c)) #(a b c) :end 2))))
(define-test sacla-must-sequence.690 (:tag :sacla)
 (assert-true (null (find-if #'(lambda (x) (eq x 'a)) #((a) (b) (c))))))
(define-test sacla-must-sequence.691 (:tag :sacla)
 (assert-true
  (equal (find-if #'(lambda (x) (eq x 'a)) #((a) (b) (c)) :key #'car) '(a))))
(define-test sacla-must-sequence.692 (:tag :sacla)
 (assert-true
  (equal (find-if #'(lambda (x) (eq x 'b)) #((a) (b) (c)) :key #'car) '(b))))
(define-test sacla-must-sequence.693 (:tag :sacla)
 (assert-true
  (equal (find-if #'(lambda (x) (eq x 'c)) #((a) (b) (c)) :key #'car) '(c))))
(define-test sacla-must-sequence.694 (:tag :sacla)
 (assert-true
  (null (find-if #'(lambda (x) (eq x 'z)) #((a) (b) (c)) :key #'car))))
(define-test sacla-must-sequence.695 (:tag :sacla)
 (assert-true
  (let ((vector #((a) (b) (c))))
    (and
     (eq (find-if #'(lambda (x) (eq x 'a)) vector :key #'car) (aref vector 0))
     (eq (find-if #'(lambda (x) (eq x 'b)) vector :key #'car) (aref vector 1))
     (eq (find-if #'(lambda (x) (eq x 'c)) vector :key #'car) (aref vector 2))
     (null (find-if #'(lambda (x) (eq x 'z)) vector :key #'car))))))
(define-test sacla-must-sequence.696 (:tag :sacla)
 (assert-true
  (equal
   (find-if #'(lambda (x) (eq x 'a))
            #((a) (b) (c) (a a) (b b) (c c))
            :key #'car)
   '(a))))
(define-test sacla-must-sequence.697 (:tag :sacla)
 (assert-true
  (equal
   (find-if #'(lambda (x) (eq x 'a))
            #((a) (b) (c) (a a) (b b) (c c))
            :key #'car
            :from-end t)
   '(a a))))
(define-test sacla-must-sequence.698 (:tag :sacla)
 (assert-true
  (equal
   (find-if #'(lambda (x) (eq x 'b))
            #((a) (b) (c) (a a) (b b) (c c))
            :key #'car)
   '(b))))
(define-test sacla-must-sequence.699 (:tag :sacla)
 (assert-true
  (equal
   (find-if #'(lambda (x) (eq x 'b))
            #((a) (b) (c) (a a) (b b) (c c))
            :key #'car
            :from-end t)
   '(b b))))
(define-test sacla-must-sequence.700 (:tag :sacla)
 (assert-true
  (equal
   (find-if #'(lambda (x) (eq x 'c))
            #((a) (b) (c) (a a) (b b) (c c))
            :key #'car)
   '(c))))
(define-test sacla-must-sequence.701 (:tag :sacla)
 (assert-true
  (equal
   (find-if #'(lambda (x) (eq x 'c))
            #((a) (b) (c) (a a) (b b) (c c))
            :key #'car
            :from-end t)
   '(c c))))
(define-test sacla-must-sequence.702 (:tag :sacla)
 (assert-true
  (null
   (find-if #'(lambda (x) (eq x 'z))
            #((a) (b) (c) (a a) (b b) (c c))
            :key #'car))))
(define-test sacla-must-sequence.703 (:tag :sacla)
 (assert-true
  (null
   (find-if #'(lambda (x) (eq x 'z))
            #((a) (b) (c) (a a) (b b) (c c))
            :key #'car
            :from-end t))))
(define-test sacla-must-sequence.704 (:tag :sacla)
 (assert-true
  (equal
   (find-if #'(lambda (x) (eq x 'a))
            #((a) (b) (c) (a a) (b b) (c c) (a a a))
            :key #'car
            :from-end t)
   '(a a a))))
(define-test sacla-must-sequence.705 (:tag :sacla)
 (assert-true
  (equal
   (find-if #'(lambda (x) (eq x 'a))
            #((a) (b) (c) (a a) (b b) (c c) (a a a))
            :key #'car
            :from-end t
            :end nil)
   '(a a a))))
(define-test sacla-must-sequence.706 (:tag :sacla)
 (assert-true
  (equal
   (find-if #'(lambda (x) (eq x 'a))
            #((a) (b) (c) (a a) (b b) (c c) (a a a))
            :key #'car
            :from-end t
            :end 6)
   '(a a))))
(define-test sacla-must-sequence.707 (:tag :sacla)
 (assert-true
  (null
   (find-if #'(lambda (x) (eq x 'a))
            #((a) (b) (c) (a a) (b b) (c c) (a a a))
            :key #'car
            :from-end t
            :start 1
            :end 3))))
(define-test sacla-must-sequence.708 (:tag :sacla)
 (assert-true (null (find-if #'(lambda (x) (eql x #\c)) #("abc" "bcd" "cde")))))
(define-test sacla-must-sequence.709 (:tag :sacla)
 (assert-true
  (string=
   (find-if #'(lambda (x) (eql x #\c))
            #("abc" "bcd" "cde")
            :key #'(lambda (arg) (char arg 0)))
   "cde")))
(define-test sacla-must-sequence.710 (:tag :sacla)
 (assert-true
  (string=
   (find-if #'(lambda (x) (char> #\c x))
            #("abc" "bcd" "cde")
            :key #'(lambda (arg) (char arg 0)))
   "abc")))
(define-test sacla-must-sequence.711 (:tag :sacla)
 (assert-true
  (string=
   (find-if #'(lambda (x) (char> #\c x))
            #("abc" "bcd" "cde")
            :start 1
            :key #'(lambda (arg) (char arg 0)))
   "bcd")))
(define-test sacla-must-sequence.712 (:tag :sacla)
 (assert-true (eq (find-if-not #'(lambda (x) (not (eq x 'a))) '(a b c)) 'a)))
(define-test sacla-must-sequence.713 (:tag :sacla)
 (assert-true (eq (find-if-not #'(lambda (x) (not (eq x 'b))) '(a b c)) 'b)))
(define-test sacla-must-sequence.714 (:tag :sacla)
 (assert-true (eq (find-if-not #'(lambda (x) (not (eq x 'c))) '(a b c)) 'c)))
(define-test sacla-must-sequence.715 (:tag :sacla)
 (assert-true (null (find-if-not #'(lambda (arg) (not (eq arg 'x))) '(a b c)))))
(define-test sacla-must-sequence.716 (:tag :sacla)
 (assert-true
  (null (find-if-not #'(lambda (x) (not (eq x 'a))) '(a b c) :start 1))))
(define-test sacla-must-sequence.717 (:tag :sacla)
 (assert-true
  (null (find-if-not #'(lambda (x) (not (eq x 'b))) '(a b c) :start 2))))
(define-test sacla-must-sequence.718 (:tag :sacla)
 (assert-true
  (null (find-if-not #'(lambda (x) (not (eq x 'c))) '(a b c) :start 3))))
(define-test sacla-must-sequence.719 (:tag :sacla)
 (assert-true
  (null (find-if-not #'(lambda (x) (not (eq x 'a))) '(a b c) :start 0 :end 0))))
(define-test sacla-must-sequence.720 (:tag :sacla)
 (assert-true
  (null
   (find-if-not #'(lambda (x) (not (eq x 'a)))
                '(a b c)
                :start 0
                :end 0
                :from-end t))))
(define-test sacla-must-sequence.721 (:tag :sacla)
 (assert-true
  (null (find-if-not #'(lambda (x) (not (eq x 'a))) '(a b c) :start 1 :end 1))))
(define-test sacla-must-sequence.722 (:tag :sacla)
 (assert-true
  (null
   (find-if-not #'(lambda (x) (not (eq x 'a)))
                '(a b c)
                :start 1
                :end 1
                :from-end t))))
(define-test sacla-must-sequence.723 (:tag :sacla)
 (assert-true
  (null (find-if-not #'(lambda (x) (not (eq x 'a))) '(a b c) :start 2 :end 2))))
(define-test sacla-must-sequence.724 (:tag :sacla)
 (assert-true
  (null
   (find-if-not #'(lambda (x) (not (eq x 'a)))
                '(a b c)
                :start 2
                :end 2
                :from-end t))))
(define-test sacla-must-sequence.725 (:tag :sacla)
 (assert-true
  (null (find-if-not #'(lambda (x) (not (eq x 'a))) '(a b c) :start 3 :end 3))))
(define-test sacla-must-sequence.726 (:tag :sacla)
 (assert-true
  (null
   (find-if-not #'(lambda (x) (not (eq x 'a)))
                '(a b c)
                :start 3
                :end 3
                :from-end t))))
(define-test sacla-must-sequence.727 (:tag :sacla)
 (assert-true
  (eq (find-if-not #'(lambda (x) (not (eq x 'a))) '(a b c) :end nil) 'a)))
(define-test sacla-must-sequence.728 (:tag :sacla)
 (assert-true
  (eq (find-if-not #'(lambda (x) (not (eq x 'b))) '(a b c) :end nil) 'b)))
(define-test sacla-must-sequence.729 (:tag :sacla)
 (assert-true
  (eq (find-if-not #'(lambda (x) (not (eq x 'c))) '(a b c) :end nil) 'c)))
(define-test sacla-must-sequence.730 (:tag :sacla)
 (assert-true
  (eq (find-if-not #'(lambda (x) (not (eq x 'a))) '(a b c) :end 1) 'a)))
(define-test sacla-must-sequence.731 (:tag :sacla)
 (assert-true
  (eq (find-if-not #'(lambda (x) (not (eq x 'b))) '(a b c) :end 2) 'b)))
(define-test sacla-must-sequence.732 (:tag :sacla)
 (assert-true
  (eq (find-if-not #'(lambda (x) (not (eq x 'c))) '(a b c) :end 3) 'c)))
(define-test sacla-must-sequence.733 (:tag :sacla)
 (assert-true
  (null (find-if-not #'(lambda (x) (not (eq x 'a))) '(a b c) :end 0))))
(define-test sacla-must-sequence.734 (:tag :sacla)
 (assert-true
  (null (find-if-not #'(lambda (x) (not (eq x 'b))) '(a b c) :end 1))))
(define-test sacla-must-sequence.735 (:tag :sacla)
 (assert-true
  (null (find-if-not #'(lambda (x) (not (eq x 'c))) '(a b c) :end 2))))
(define-test sacla-must-sequence.736 (:tag :sacla)
 (assert-true
  (null (find-if-not #'(lambda (x) (not (eq x 'a))) '((a) (b) (c))))))
(define-test sacla-must-sequence.737 (:tag :sacla)
 (assert-true
  (equal (find-if-not #'(lambda (x) (not (eq x 'a))) '((a) (b) (c)) :key #'car)
         '(a))))
(define-test sacla-must-sequence.738 (:tag :sacla)
 (assert-true
  (equal (find-if-not #'(lambda (x) (not (eq x 'b))) '((a) (b) (c)) :key #'car)
         '(b))))
(define-test sacla-must-sequence.739 (:tag :sacla)
 (assert-true
  (equal (find-if-not #'(lambda (x) (not (eq x 'c))) '((a) (b) (c)) :key #'car)
         '(c))))
(define-test sacla-must-sequence.740 (:tag :sacla)
 (assert-true
  (null
   (find-if-not #'(lambda (x) (not (eq x 'z))) '((a) (b) (c)) :key #'car))))
(define-test sacla-must-sequence.741 (:tag :sacla)
 (assert-true
  (let ((list '((a) (b) (c))))
    (and
     (eq (find-if-not #'(lambda (x) (not (eq x 'a))) list :key #'car)
         (car list))
     (eq (find-if-not #'(lambda (x) (not (eq x 'b))) list :key #'car)
         (cadr list))
     (eq (find-if-not #'(lambda (x) (not (eq x 'c))) list :key #'car)
         (caddr list))
     (null (find-if-not #'(lambda (x) (not (eq x 'z))) list :key #'car))))))
(define-test sacla-must-sequence.742 (:tag :sacla)
 (assert-true
  (equal
   (find-if-not #'(lambda (x) (not (eq x 'a)))
                '((a) (b) (c) (a a) (b b) (c c))
                :key #'car)
   '(a))))
(define-test sacla-must-sequence.743 (:tag :sacla)
 (assert-true
  (equal
   (find-if-not #'(lambda (x) (not (eq x 'a)))
                '((a) (b) (c) (a a) (b b) (c c))
                :key #'car
                :from-end t)
   '(a a))))
(define-test sacla-must-sequence.744 (:tag :sacla)
 (assert-true
  (equal
   (find-if-not #'(lambda (x) (not (eq x 'b)))
                '((a) (b) (c) (a a) (b b) (c c))
                :key #'car)
   '(b))))
(define-test sacla-must-sequence.745 (:tag :sacla)
 (assert-true
  (equal
   (find-if-not #'(lambda (x) (not (eq x 'b)))
                '((a) (b) (c) (a a) (b b) (c c))
                :key #'car
                :from-end t)
   '(b b))))
(define-test sacla-must-sequence.746 (:tag :sacla)
 (assert-true
  (equal
   (find-if-not #'(lambda (x) (not (eq x 'c)))
                '((a) (b) (c) (a a) (b b) (c c))
                :key #'car)
   '(c))))
(define-test sacla-must-sequence.747 (:tag :sacla)
 (assert-true
  (equal
   (find-if-not #'(lambda (x) (not (eq x 'c)))
                '((a) (b) (c) (a a) (b b) (c c))
                :key #'car
                :from-end t)
   '(c c))))
(define-test sacla-must-sequence.748 (:tag :sacla)
 (assert-true
  (null
   (find-if-not #'(lambda (x) (not (eq x 'z)))
                '((a) (b) (c) (a a) (b b) (c c))
                :key #'car))))
(define-test sacla-must-sequence.749 (:tag :sacla)
 (assert-true
  (null
   (find-if-not #'(lambda (x) (not (eq x 'z)))
                '((a) (b) (c) (a a) (b b) (c c))
                :key #'car
                :from-end t))))
(define-test sacla-must-sequence.750 (:tag :sacla)
 (assert-true
  (equal
   (find-if-not #'(lambda (x) (not (eq x 'a)))
                '((a) (b) (c) (a a) (b b) (c c) (a a a))
                :key #'car
                :from-end t)
   '(a a a))))
(define-test sacla-must-sequence.751 (:tag :sacla)
 (assert-true
  (equal
   (find-if-not #'(lambda (x) (not (eq x 'a)))
                '((a) (b) (c) (a a) (b b) (c c) (a a a))
                :key #'car
                :from-end t
                :end nil)
   '(a a a))))
(define-test sacla-must-sequence.752 (:tag :sacla)
 (assert-true
  (equal
   (find-if-not #'(lambda (x) (not (eq x 'a)))
                '((a) (b) (c) (a a) (b b) (c c) (a a a))
                :key #'car
                :from-end t
                :end 6)
   '(a a))))
(define-test sacla-must-sequence.753 (:tag :sacla)
 (assert-true
  (null
   (find-if-not #'(lambda (x) (not (eq x 'a)))
                '((a) (b) (c) (a a) (b b) (c c) (a a a))
                :key #'car
                :from-end t
                :start 1
                :end 3))))
(define-test sacla-must-sequence.754 (:tag :sacla)
 (assert-true
  (null (find-if-not #'(lambda (x) (not (eql x #\c))) '("abc" "bcd" "cde")))))
(define-test sacla-must-sequence.755 (:tag :sacla)
 (assert-true
  (string=
   (find-if-not #'(lambda (x) (not (eql x #\c)))
                '("abc" "bcd" "cde")
                :key #'(lambda (arg) (char arg 0)))
   "cde")))
(define-test sacla-must-sequence.756 (:tag :sacla)
 (assert-true
  (string=
   (find-if-not #'(lambda (x) (not (char> #\c x)))
                '("abc" "bcd" "cde")
                :key #'(lambda (arg) (char arg 0)))
   "abc")))
(define-test sacla-must-sequence.757 (:tag :sacla)
 (assert-true
  (string=
   (find-if-not #'(lambda (x) (not (char> #\c x)))
                '("abc" "bcd" "cde")
                :start 1
                :key #'(lambda (arg) (char arg 0)))
   "bcd")))
(define-test sacla-must-sequence.758 (:tag :sacla)
 (assert-true (eq (find-if-not #'(lambda (x) (not (eq x 'a))) #(a b c)) 'a)))
(define-test sacla-must-sequence.759 (:tag :sacla)
 (assert-true (eq (find-if-not #'(lambda (x) (not (eq x 'b))) #(a b c)) 'b)))
(define-test sacla-must-sequence.760 (:tag :sacla)
 (assert-true (eq (find-if-not #'(lambda (x) (not (eq x 'c))) #(a b c)) 'c)))
(define-test sacla-must-sequence.761 (:tag :sacla)
 (assert-true (null (find-if-not #'(lambda (arg) (not (eq arg 'x))) #(a b c)))))
(define-test sacla-must-sequence.762 (:tag :sacla)
 (assert-true
  (null (find-if-not #'(lambda (x) (not (eq x 'a))) #(a b c) :start 1))))
(define-test sacla-must-sequence.763 (:tag :sacla)
 (assert-true
  (null (find-if-not #'(lambda (x) (not (eq x 'b))) #(a b c) :start 2))))
(define-test sacla-must-sequence.764 (:tag :sacla)
 (assert-true
  (null (find-if-not #'(lambda (x) (not (eq x 'c))) #(a b c) :start 3))))
(define-test sacla-must-sequence.765 (:tag :sacla)
 (assert-true
  (null (find-if-not #'(lambda (x) (not (eq x 'a))) #(a b c) :start 0 :end 0))))
(define-test sacla-must-sequence.766 (:tag :sacla)
 (assert-true
  (null
   (find-if-not #'(lambda (x) (not (eq x 'a)))
                #(a b c)
                :start 0
                :end 0
                :from-end t))))
(define-test sacla-must-sequence.767 (:tag :sacla)
 (assert-true
  (null (find-if-not #'(lambda (x) (not (eq x 'a))) #(a b c) :start 1 :end 1))))
(define-test sacla-must-sequence.768 (:tag :sacla)
 (assert-true
  (null
   (find-if-not #'(lambda (x) (not (eq x 'a)))
                #(a b c)
                :start 1
                :end 1
                :from-end t))))
(define-test sacla-must-sequence.769 (:tag :sacla)
 (assert-true
  (null (find-if-not #'(lambda (x) (not (eq x 'a))) #(a b c) :start 2 :end 2))))
(define-test sacla-must-sequence.770 (:tag :sacla)
 (assert-true
  (null
   (find-if-not #'(lambda (x) (not (eq x 'a)))
                #(a b c)
                :start 2
                :end 2
                :from-end t))))
(define-test sacla-must-sequence.771 (:tag :sacla)
 (assert-true
  (null (find-if-not #'(lambda (x) (not (eq x 'a))) #(a b c) :start 3 :end 3))))
(define-test sacla-must-sequence.772 (:tag :sacla)
 (assert-true
  (null
   (find-if-not #'(lambda (x) (not (eq x 'a)))
                #(a b c)
                :start 3
                :end 3
                :from-end t))))
(define-test sacla-must-sequence.773 (:tag :sacla)
 (assert-true
  (eq (find-if-not #'(lambda (x) (not (eq x 'a))) #(a b c) :end nil) 'a)))
(define-test sacla-must-sequence.774 (:tag :sacla)
 (assert-true
  (eq (find-if-not #'(lambda (x) (not (eq x 'b))) #(a b c) :end nil) 'b)))
(define-test sacla-must-sequence.775 (:tag :sacla)
 (assert-true
  (eq (find-if-not #'(lambda (x) (not (eq x 'c))) #(a b c) :end nil) 'c)))
(define-test sacla-must-sequence.776 (:tag :sacla)
 (assert-true
  (eq (find-if-not #'(lambda (x) (not (eq x 'a))) #(a b c) :end 1) 'a)))
(define-test sacla-must-sequence.777 (:tag :sacla)
 (assert-true
  (eq (find-if-not #'(lambda (x) (not (eq x 'b))) #(a b c) :end 2) 'b)))
(define-test sacla-must-sequence.778 (:tag :sacla)
 (assert-true
  (eq (find-if-not #'(lambda (x) (not (eq x 'c))) #(a b c) :end 3) 'c)))
(define-test sacla-must-sequence.779 (:tag :sacla)
 (assert-true
  (null (find-if-not #'(lambda (x) (not (eq x 'a))) #(a b c) :end 0))))
(define-test sacla-must-sequence.780 (:tag :sacla)
 (assert-true
  (null (find-if-not #'(lambda (x) (not (eq x 'b))) #(a b c) :end 1))))
(define-test sacla-must-sequence.781 (:tag :sacla)
 (assert-true
  (null (find-if-not #'(lambda (x) (not (eq x 'c))) #(a b c) :end 2))))
(define-test sacla-must-sequence.782 (:tag :sacla)
 (assert-true
  (null (find-if-not #'(lambda (x) (not (eq x 'a))) #((a) (b) (c))))))
(define-test sacla-must-sequence.783 (:tag :sacla)
 (assert-true
  (equal (find-if-not #'(lambda (x) (not (eq x 'a))) #((a) (b) (c)) :key #'car)
         '(a))))
(define-test sacla-must-sequence.784 (:tag :sacla)
 (assert-true
  (equal (find-if-not #'(lambda (x) (not (eq x 'b))) #((a) (b) (c)) :key #'car)
         '(b))))
(define-test sacla-must-sequence.785 (:tag :sacla)
 (assert-true
  (equal (find-if-not #'(lambda (x) (not (eq x 'c))) #((a) (b) (c)) :key #'car)
         '(c))))
(define-test sacla-must-sequence.786 (:tag :sacla)
 (assert-true
  (null
   (find-if-not #'(lambda (x) (not (eq x 'z))) #((a) (b) (c)) :key #'car))))
(define-test sacla-must-sequence.787 (:tag :sacla)
 (assert-true
  (let ((vector #((a) (b) (c))))
    (and
     (eq (find-if-not #'(lambda (x) (not (eq x 'a))) vector :key #'car)
         (aref vector 0))
     (eq (find-if-not #'(lambda (x) (not (eq x 'b))) vector :key #'car)
         (aref vector 1))
     (eq (find-if-not #'(lambda (x) (not (eq x 'c))) vector :key #'car)
         (aref vector 2))
     (null (find-if-not #'(lambda (x) (not (eq x 'z))) vector :key #'car))))))
(define-test sacla-must-sequence.788 (:tag :sacla)
 (assert-true
  (equal
   (find-if-not #'(lambda (x) (not (eq x 'a)))
                #((a) (b) (c) (a a) (b b) (c c))
                :key #'car)
   '(a))))
(define-test sacla-must-sequence.789 (:tag :sacla)
 (assert-true
  (equal
   (find-if-not #'(lambda (x) (not (eq x 'a)))
                #((a) (b) (c) (a a) (b b) (c c))
                :key #'car
                :from-end t)
   '(a a))))
(define-test sacla-must-sequence.790 (:tag :sacla)
 (assert-true
  (equal
   (find-if-not #'(lambda (x) (not (eq x 'b)))
                #((a) (b) (c) (a a) (b b) (c c))
                :key #'car)
   '(b))))
(define-test sacla-must-sequence.791 (:tag :sacla)
 (assert-true
  (equal
   (find-if-not #'(lambda (x) (not (eq x 'b)))
                #((a) (b) (c) (a a) (b b) (c c))
                :key #'car
                :from-end t)
   '(b b))))
(define-test sacla-must-sequence.792 (:tag :sacla)
 (assert-true
  (equal
   (find-if-not #'(lambda (x) (not (eq x 'c)))
                #((a) (b) (c) (a a) (b b) (c c))
                :key #'car)
   '(c))))
(define-test sacla-must-sequence.793 (:tag :sacla)
 (assert-true
  (equal
   (find-if-not #'(lambda (x) (not (eq x 'c)))
                #((a) (b) (c) (a a) (b b) (c c))
                :key #'car
                :from-end t)
   '(c c))))
(define-test sacla-must-sequence.794 (:tag :sacla)
 (assert-true
  (null
   (find-if-not #'(lambda (x) (not (eq x 'z)))
                #((a) (b) (c) (a a) (b b) (c c))
                :key #'car))))
(define-test sacla-must-sequence.795 (:tag :sacla)
 (assert-true
  (null
   (find-if-not #'(lambda (x) (not (eq x 'z)))
                #((a) (b) (c) (a a) (b b) (c c))
                :key #'car
                :from-end t))))
(define-test sacla-must-sequence.796 (:tag :sacla)
 (assert-true
  (equal
   (find-if-not #'(lambda (x) (not (eq x 'a)))
                #((a) (b) (c) (a a) (b b) (c c) (a a a))
                :key #'car
                :from-end t)
   '(a a a))))
(define-test sacla-must-sequence.797 (:tag :sacla)
 (assert-true
  (equal
   (find-if-not #'(lambda (x) (not (eq x 'a)))
                #((a) (b) (c) (a a) (b b) (c c) (a a a))
                :key #'car
                :from-end t
                :end nil)
   '(a a a))))
(define-test sacla-must-sequence.798 (:tag :sacla)
 (assert-true
  (equal
   (find-if-not #'(lambda (x) (not (eq x 'a)))
                #((a) (b) (c) (a a) (b b) (c c) (a a a))
                :key #'car
                :from-end t
                :end 6)
   '(a a))))
(define-test sacla-must-sequence.799 (:tag :sacla)
 (assert-true
  (null
   (find-if-not #'(lambda (x) (not (eq x 'a)))
                #((a) (b) (c) (a a) (b b) (c c) (a a a))
                :key #'car
                :from-end t
                :start 1
                :end 3))))
(define-test sacla-must-sequence.800 (:tag :sacla)
 (assert-true
  (string=
   (find-if-not #'(lambda (x) (not (eql x #\c)))
                #("abc" "bcd" "cde")
                :key #'(lambda (arg) (char arg 0)))
   "cde")))
(define-test sacla-must-sequence.801 (:tag :sacla)
 (assert-true
  (string=
   (find-if-not #'(lambda (x) (not (char> #\c x)))
                #("abc" "bcd" "cde")
                :key #'(lambda (arg) (char arg 0)))
   "abc")))
(define-test sacla-must-sequence.802 (:tag :sacla)
 (assert-true
  (string=
   (find-if-not #'(lambda (x) (not (char> #\c x)))
                #("abc" "bcd" "cde")
                :start 1
                :key #'(lambda (arg) (char arg 0)))
   "bcd")))
(define-test sacla-must-sequence.803 (:tag :sacla)
 (assert-true (eql (position #\a "baobab" :from-end t) 4)))
(define-test sacla-must-sequence.804 (:tag :sacla)
 (assert-true
  (eql (position-if #'oddp '((1) (2) (3) (4)) :start 1 :key #'car) 2)))
(define-test sacla-must-sequence.805 (:tag :sacla)
 (assert-true (null (position 595 'nil))))
(define-test sacla-must-sequence.806 (:tag :sacla)
 (assert-true (eql (position-if-not #'integerp '(1 2 3 4 5.0)) 4)))
(define-test sacla-must-sequence.807 (:tag :sacla)
 (assert-true (eql (position 'a '(a b c)) 0)))
(define-test sacla-must-sequence.808 (:tag :sacla)
 (assert-true (eql (position 'b '(a b c)) 1)))
(define-test sacla-must-sequence.809 (:tag :sacla)
 (assert-true (eql (position 'c '(a b c)) 2)))
(define-test sacla-must-sequence.810 (:tag :sacla)
 (assert-true (null (position 'x '(a b c)))))
(define-test sacla-must-sequence.811 (:tag :sacla)
 (assert-true (null (position 'a '(a b c) :start 1))))
(define-test sacla-must-sequence.812 (:tag :sacla)
 (assert-true (null (position 'b '(a b c) :start 2))))
(define-test sacla-must-sequence.813 (:tag :sacla)
 (assert-true (null (position 'c '(a b c) :start 3))))
(define-test sacla-must-sequence.814 (:tag :sacla)
 (assert-true (null (position 'a '(a b c) :start 0 :end 0))))
(define-test sacla-must-sequence.815 (:tag :sacla)
 (assert-true (null (position 'a '(a b c) :start 0 :end 0 :from-end t))))
(define-test sacla-must-sequence.816 (:tag :sacla)
 (assert-true (null (position 'a '(a b c) :start 1 :end 1))))
(define-test sacla-must-sequence.817 (:tag :sacla)
 (assert-true (null (position 'a '(a b c) :start 1 :end 1 :from-end t))))
(define-test sacla-must-sequence.818 (:tag :sacla)
 (assert-true (null (position 'a '(a b c) :start 2 :end 2))))
(define-test sacla-must-sequence.819 (:tag :sacla)
 (assert-true (null (position 'a '(a b c) :start 2 :end 2 :from-end t))))
(define-test sacla-must-sequence.820 (:tag :sacla)
 (assert-true (null (position 'a '(a b c) :start 3 :end 3))))
(define-test sacla-must-sequence.821 (:tag :sacla)
 (assert-true (null (position 'a '(a b c) :start 3 :end 3 :from-end t))))
(define-test sacla-must-sequence.822 (:tag :sacla)
 (assert-true (eql (position 'a '(a b c) :end nil) '0)))
(define-test sacla-must-sequence.823 (:tag :sacla)
 (assert-true (eql (position 'b '(a b c) :end nil) '1)))
(define-test sacla-must-sequence.824 (:tag :sacla)
 (assert-true (eql (position 'c '(a b c) :end nil) '2)))
(define-test sacla-must-sequence.825 (:tag :sacla)
 (assert-true (eql (position 'a '(a b c) :end 1) '0)))
(define-test sacla-must-sequence.826 (:tag :sacla)
 (assert-true (eql (position 'b '(a b c) :end 2) '1)))
(define-test sacla-must-sequence.827 (:tag :sacla)
 (assert-true (eql (position 'c '(a b c) :end 3) '2)))
(define-test sacla-must-sequence.828 (:tag :sacla)
 (assert-true (null (position 'a '(a b c) :end 0))))
(define-test sacla-must-sequence.829 (:tag :sacla)
 (assert-true (null (position 'b '(a b c) :end 1))))
(define-test sacla-must-sequence.830 (:tag :sacla)
 (assert-true (null (position 'c '(a b c) :end 2))))
(define-test sacla-must-sequence.831 (:tag :sacla)
 (assert-true (null (position 'a '((a) (b) (c))))))
(define-test sacla-must-sequence.832 (:tag :sacla)
 (assert-true (eql (position 'a '((a) (b) (c)) :key #'car) 0)))
(define-test sacla-must-sequence.833 (:tag :sacla)
 (assert-true (eql (position 'b '((a) (b) (c)) :key #'car) 1)))
(define-test sacla-must-sequence.834 (:tag :sacla)
 (assert-true (eql (position 'c '((a) (b) (c)) :key #'car) 2)))
(define-test sacla-must-sequence.835 (:tag :sacla)
 (assert-true (null (position 'z '((a) (b) (c)) :key #'car))))
(define-test sacla-must-sequence.836 (:tag :sacla)
 (assert-true (null (position '(a) '((a) (b) (c))))))
(define-test sacla-must-sequence.837 (:tag :sacla)
 (assert-true (eql (position '(a) '((a) (b) (c)) :test #'equal) 0)))
(define-test sacla-must-sequence.838 (:tag :sacla)
 (assert-true (null (position '("a") '(("a") ("b") ("c"))))))
(define-test sacla-must-sequence.839 (:tag :sacla)
 (assert-true (null (position '("a") '(("A") ("B") ("c")) :test #'equal))))
(define-test sacla-must-sequence.840 (:tag :sacla)
 (assert-true (eql (position '("a") '(("A") ("B") ("c")) :test #'equalp) 0)))
(define-test sacla-must-sequence.841 (:tag :sacla)
 (assert-true
  (eql (position 'nil '(first second third) :test (constantly t)) 0)))
(define-test sacla-must-sequence.842 (:tag :sacla)
 (assert-true (eql (position 3 '(0 1 2 3 4 5)) 3)))
(define-test sacla-must-sequence.843 (:tag :sacla)
 (assert-true (eql (position 3 '(0 1 2 3 4 5) :test #'<) 4)))
(define-test sacla-must-sequence.844 (:tag :sacla)
 (assert-true (eql (position 3 '(0 1 2 3 4 5) :test #'>) 0)))
(define-test sacla-must-sequence.845 (:tag :sacla)
 (assert-true
  (eql (position '(a) '((a) (b) (c)) :test-not (complement #'equal)) 0)))
(define-test sacla-must-sequence.846 (:tag :sacla)
 (assert-true
  (null (position '("a") '(("A") ("B") ("c")) :test-not (complement #'equal)))))
(define-test sacla-must-sequence.847 (:tag :sacla)
 (assert-true
  (eql (position '("a") '(("A") ("B") ("c")) :test-not (complement #'equalp))
       0)))
(define-test sacla-must-sequence.848 (:tag :sacla)
 (assert-true
  (eql (position 'nil '(first second third) :test-not (constantly nil)) 0)))
(define-test sacla-must-sequence.849 (:tag :sacla)
 (assert-true (eql (position 3 '(0 1 2 3 4 5) :test-not #'>=) 4)))
(define-test sacla-must-sequence.850 (:tag :sacla)
 (assert-true (eql (position 3 '(0 1 2 3 4 5) :test-not #'<=) 0)))
(define-test sacla-must-sequence.851 (:tag :sacla)
 (assert-true
  (eql (position 'a '((a) (b) (c) (a a) (b b) (c c)) :key #'car) 0)))
(define-test sacla-must-sequence.852 (:tag :sacla)
 (assert-true
  (eql (position 'a '((a) (b) (c) (a a) (b b) (c c)) :key #'car :from-end t)
       3)))
(define-test sacla-must-sequence.853 (:tag :sacla)
 (assert-true
  (eql (position 'b '((a) (b) (c) (a a) (b b) (c c)) :key #'car) 1)))
(define-test sacla-must-sequence.854 (:tag :sacla)
 (assert-true
  (eql (position 'b '((a) (b) (c) (a a) (b b) (c c)) :key #'car :from-end t)
       4)))
(define-test sacla-must-sequence.855 (:tag :sacla)
 (assert-true
  (eql (position 'c '((a) (b) (c) (a a) (b b) (c c)) :key #'car) 2)))
(define-test sacla-must-sequence.856 (:tag :sacla)
 (assert-true
  (eql (position 'c '((a) (b) (c) (a a) (b b) (c c)) :key #'car :from-end t)
       5)))
(define-test sacla-must-sequence.857 (:tag :sacla)
 (assert-true (null (position 'z '((a) (b) (c) (a a) (b b) (c c)) :key #'car))))
(define-test sacla-must-sequence.858 (:tag :sacla)
 (assert-true
  (null (position 'z '((a) (b) (c) (a a) (b b) (c c)) :key #'car :from-end t))))
(define-test sacla-must-sequence.859 (:tag :sacla)
 (assert-true
  (eql
   (position 'a
             '((a) (b) (c) (a a) (b b) (c c) (a a a))
             :key #'car
             :from-end t)
   6)))
(define-test sacla-must-sequence.860 (:tag :sacla)
 (assert-true
  (eql
   (position 'a
             '((a) (b) (c) (a a) (b b) (c c) (a a a))
             :key #'car
             :from-end t
             :end nil)
   6)))
(define-test sacla-must-sequence.861 (:tag :sacla)
 (assert-true
  (eql
   (position 'a
             '((a) (b) (c) (a a) (b b) (c c) (a a a))
             :key #'car
             :from-end t
             :end 6)
   3)))
(define-test sacla-must-sequence.862 (:tag :sacla)
 (assert-true
  (null
   (position 'a
             '((a) (b) (c) (a a) (b b) (c c) (a a a))
             :key #'car
             :from-end t
             :start 1
             :end 3))))
(define-test sacla-must-sequence.863 (:tag :sacla)
 (assert-true (null (position #\c '("abc" "bcd" "cde")))))
(define-test sacla-must-sequence.864 (:tag :sacla)
 (assert-true
  (eql
   (position #\c
             '("abc" "bcd" "cde")
             :key #'(lambda (arg) (char arg 0))
             :test #'char=)
   2)))
(define-test sacla-must-sequence.865 (:tag :sacla)
 (assert-true
  (eql
   (position #\c
             '("abc" "bcd" "cde")
             :key #'(lambda (arg) (char arg 0))
             :test #'char>)
   0)))
(define-test sacla-must-sequence.866 (:tag :sacla)
 (assert-true
  (eql
   (position #\c
             '("abc" "bcd" "cde")
             :start 1
             :key #'(lambda (arg) (char arg 0))
             :test #'char>)
   1)))
(define-test sacla-must-sequence.867 (:tag :sacla)
 (assert-true (eql (position 'a #(a b c)) 0)))
(define-test sacla-must-sequence.868 (:tag :sacla)
 (assert-true (eql (position 'b #(a b c)) 1)))
(define-test sacla-must-sequence.869 (:tag :sacla)
 (assert-true (eql (position 'c #(a b c)) 2)))
(define-test sacla-must-sequence.870 (:tag :sacla)
 (assert-true (null (position 'x #(a b c)))))
(define-test sacla-must-sequence.871 (:tag :sacla)
 (assert-true (null (position 'a #(a b c) :start 1))))
(define-test sacla-must-sequence.872 (:tag :sacla)
 (assert-true (null (position 'b #(a b c) :start 2))))
(define-test sacla-must-sequence.873 (:tag :sacla)
 (assert-true (null (position 'c #(a b c) :start 3))))
(define-test sacla-must-sequence.874 (:tag :sacla)
 (assert-true (null (position 'a #(a b c) :start 0 :end 0))))
(define-test sacla-must-sequence.875 (:tag :sacla)
 (assert-true (null (position 'a #(a b c) :start 0 :end 0 :from-end t))))
(define-test sacla-must-sequence.876 (:tag :sacla)
 (assert-true (null (position 'a #(a b c) :start 1 :end 1))))
(define-test sacla-must-sequence.877 (:tag :sacla)
 (assert-true (null (position 'a #(a b c) :start 1 :end 1 :from-end t))))
(define-test sacla-must-sequence.878 (:tag :sacla)
 (assert-true (null (position 'a #(a b c) :start 2 :end 2))))
(define-test sacla-must-sequence.879 (:tag :sacla)
 (assert-true (null (position 'a #(a b c) :start 2 :end 2 :from-end t))))
(define-test sacla-must-sequence.880 (:tag :sacla)
 (assert-true (null (position 'a #(a b c) :start 3 :end 3))))
(define-test sacla-must-sequence.881 (:tag :sacla)
 (assert-true (null (position 'a #(a b c) :start 3 :end 3 :from-end t))))
(define-test sacla-must-sequence.882 (:tag :sacla)
 (assert-true (eql (position 'a #(a b c) :end nil) 0)))
(define-test sacla-must-sequence.883 (:tag :sacla)
 (assert-true (eql (position 'b #(a b c) :end nil) 1)))
(define-test sacla-must-sequence.884 (:tag :sacla)
 (assert-true (eql (position 'c #(a b c) :end nil) 2)))
(define-test sacla-must-sequence.885 (:tag :sacla)
 (assert-true (eql (position 'a #(a b c) :end 1) 0)))
(define-test sacla-must-sequence.886 (:tag :sacla)
 (assert-true (eql (position 'b #(a b c) :end 2) 1)))
(define-test sacla-must-sequence.887 (:tag :sacla)
 (assert-true (eql (position 'c #(a b c) :end 3) 2)))
(define-test sacla-must-sequence.888 (:tag :sacla)
 (assert-true (null (position 'a #(a b c) :end 0))))
(define-test sacla-must-sequence.889 (:tag :sacla)
 (assert-true (null (position 'b #(a b c) :end 1))))
(define-test sacla-must-sequence.890 (:tag :sacla)
 (assert-true (null (position 'c #(a b c) :end 2))))
(define-test sacla-must-sequence.891 (:tag :sacla)
 (assert-true (null (position 'a #((a) (b) (c))))))
(define-test sacla-must-sequence.892 (:tag :sacla)
 (assert-true (eql (position 'a #((a) (b) (c)) :key #'car) 0)))
(define-test sacla-must-sequence.893 (:tag :sacla)
 (assert-true (eql (position 'b #((a) (b) (c)) :key #'car) 1)))
(define-test sacla-must-sequence.894 (:tag :sacla)
 (assert-true (eql (position 'c #((a) (b) (c)) :key #'car) 2)))
(define-test sacla-must-sequence.895 (:tag :sacla)
 (assert-true (null (position 'z #((a) (b) (c)) :key #'car))))
(define-test sacla-must-sequence.896 (:tag :sacla)
 (assert-true (null (position '(a) #((a) (b) (c))))))
(define-test sacla-must-sequence.897 (:tag :sacla)
 (assert-true (eql (position '(a) #((a) (b) (c)) :test #'equal) 0)))
(define-test sacla-must-sequence.898 (:tag :sacla)
 (assert-true (null (position '("a") #(("a") ("b") ("c"))))))
(define-test sacla-must-sequence.899 (:tag :sacla)
 (assert-true (null (position '("a") #(("A") ("B") ("c")) :test #'equal))))
(define-test sacla-must-sequence.900 (:tag :sacla)
 (assert-true (eql (position '("a") #(("A") ("B") ("c")) :test #'equalp) 0)))
(define-test sacla-must-sequence.901 (:tag :sacla)
 (assert-true
  (eql (position 'nil #(first second third) :test (constantly t)) 0)))
(define-test sacla-must-sequence.902 (:tag :sacla)
 (assert-true
  (eql (position 'nil #(first second third) :test (constantly t) :from-end t)
       2)))
(define-test sacla-must-sequence.903 (:tag :sacla)
 (assert-true (eql (position 3 #(0 1 2 3 4 5)) 3)))
(define-test sacla-must-sequence.904 (:tag :sacla)
 (assert-true (eql (position 3 #(0 1 2 3 4 5) :test #'<) 4)))
(define-test sacla-must-sequence.905 (:tag :sacla)
 (assert-true (eql (position 3 #(0 1 2 3 4 5) :test #'>) 0)))
(define-test sacla-must-sequence.906 (:tag :sacla)
 (assert-true
  (eql (position '(a) #((a) (b) (c)) :test-not (complement #'equal)) 0)))
(define-test sacla-must-sequence.907 (:tag :sacla)
 (assert-true
  (null (position '("a") #(("A") ("B") ("c")) :test-not (complement #'equal)))))
(define-test sacla-must-sequence.908 (:tag :sacla)
 (assert-true
  (eql (position '("a") #(("A") ("B") ("c")) :test-not (complement #'equalp))
       0)))
(define-test sacla-must-sequence.909 (:tag :sacla)
 (assert-true
  (eql (position 'nil #(first second third) :test-not (constantly nil)) 0)))
(define-test sacla-must-sequence.910 (:tag :sacla)
 (assert-true (eql (position 3 #(0 1 2 3 4 5) :test-not #'>=) 4)))
(define-test sacla-must-sequence.911 (:tag :sacla)
 (assert-true (eql (position 3 #(0 1 2 3 4 5) :test-not #'<=) 0)))
(define-test sacla-must-sequence.912 (:tag :sacla)
 (assert-true
  (eql (position 'a #((a) (b) (c) (a a) (b b) (c c)) :key #'car) 0)))
(define-test sacla-must-sequence.913 (:tag :sacla)
 (assert-true
  (eql (position 'a #((a) (b) (c) (a a) (b b) (c c)) :key #'car :from-end t)
       3)))
(define-test sacla-must-sequence.914 (:tag :sacla)
 (assert-true
  (eql (position 'b #((a) (b) (c) (a a) (b b) (c c)) :key #'car) 1)))
(define-test sacla-must-sequence.915 (:tag :sacla)
 (assert-true
  (eql (position 'b #((a) (b) (c) (a a) (b b) (c c)) :key #'car :from-end t)
       4)))
(define-test sacla-must-sequence.916 (:tag :sacla)
 (assert-true
  (eql (position 'c #((a) (b) (c) (a a) (b b) (c c)) :key #'car) 2)))
(define-test sacla-must-sequence.917 (:tag :sacla)
 (assert-true
  (eql (position 'c #((a) (b) (c) (a a) (b b) (c c)) :key #'car :from-end t)
       5)))
(define-test sacla-must-sequence.918 (:tag :sacla)
 (assert-true (null (position 'z #((a) (b) (c) (a a) (b b) (c c)) :key #'car))))
(define-test sacla-must-sequence.919 (:tag :sacla)
 (assert-true
  (null (position 'z #((a) (b) (c) (a a) (b b) (c c)) :key #'car :from-end t))))
(define-test sacla-must-sequence.920 (:tag :sacla)
 (assert-true
  (eql
   (position 'a
             #((a) (b) (c) (a a) (b b) (c c) (a a a))
             :key #'car
             :from-end t)
   6)))
(define-test sacla-must-sequence.921 (:tag :sacla)
 (assert-true
  (eql
   (position 'a
             #((a) (b) (c) (a a) (b b) (c c) (a a a))
             :key #'car
             :from-end t
             :end nil)
   6)))
(define-test sacla-must-sequence.922 (:tag :sacla)
 (assert-true
  (eql
   (position 'a
             #((a) (b) (c) (a a) (b b) (c c) (a a a))
             :key #'car
             :from-end t
             :end 6)
   3)))
(define-test sacla-must-sequence.923 (:tag :sacla)
 (assert-true
  (null
   (position 'a
             #((a) (b) (c) (a a) (b b) (c c) (a a a))
             :key #'car
             :from-end t
             :start 1
             :end 3))))
(define-test sacla-must-sequence.924 (:tag :sacla)
 (assert-true (null (position #\c #("abc" "bcd" "cde")))))
(define-test sacla-must-sequence.925 (:tag :sacla)
 (assert-true
  (eql
   (position #\c
             #("abc" "bcd" "cde")
             :key #'(lambda (arg) (char arg 0))
             :test #'char=)
   2)))
(define-test sacla-must-sequence.926 (:tag :sacla)
 (assert-true
  (eql
   (position #\c
             #("abc" "bcd" "cde")
             :key #'(lambda (arg) (char arg 0))
             :test #'char>)
   0)))
(define-test sacla-must-sequence.927 (:tag :sacla)
 (assert-true
  (eql
   (position #\c
             #("abc" "bcd" "cde")
             :start 1
             :key #'(lambda (arg) (char arg 0))
             :test #'char>)
   1)))
(define-test sacla-must-sequence.928 (:tag :sacla)
 (assert-true (null (position #\z "abcABC"))))
(define-test sacla-must-sequence.929 (:tag :sacla)
 (assert-true (eql (position #\a "abcABC") 0)))
(define-test sacla-must-sequence.930 (:tag :sacla)
 (assert-true (eql (position #\A "abcABC") 3)))
(define-test sacla-must-sequence.931 (:tag :sacla)
 (assert-true (eql (position #\A "abcABC" :test #'char-equal) 0)))
(define-test sacla-must-sequence.932 (:tag :sacla)
 (assert-true (eql (position #\A "abcABC" :test #'char-equal :from-end t) 3)))
(define-test sacla-must-sequence.933 (:tag :sacla)
 (assert-true (eql (position #\a "abcABC" :test #'char-equal :from-end t) 3)))
(define-test sacla-must-sequence.934 (:tag :sacla)
 (assert-true
  (eql (position #\a "abcABC" :test #'char-equal :from-end t :end 4) 3)))
(define-test sacla-must-sequence.935 (:tag :sacla)
 (assert-true
  (eql (position #\a "abcABC" :test #'char-equal :from-end t :end 3) 0)))
(define-test sacla-must-sequence.936 (:tag :sacla)
 (assert-true (zerop (position 0 #*01))))
(define-test sacla-must-sequence.937 (:tag :sacla)
 (assert-true (eql (position 1 #*01) 1)))
(define-test sacla-must-sequence.938 (:tag :sacla)
 (assert-true (null (position 0 #*01 :start 1))))
(define-test sacla-must-sequence.939 (:tag :sacla)
 (assert-true (null (position 1 #*01 :end 0))))
(define-test sacla-must-sequence.940 (:tag :sacla)
 (assert-true (null (position 0 #*000001 :start 5))))
(define-test sacla-must-sequence.941 (:tag :sacla)
 (assert-true (eql (position-if #'(lambda (x) (eq x 'a)) '(a b c)) 0)))
(define-test sacla-must-sequence.942 (:tag :sacla)
 (assert-true (eql (position-if #'(lambda (x) (eq x 'b)) '(a b c)) 1)))
(define-test sacla-must-sequence.943 (:tag :sacla)
 (assert-true (eql (position-if #'(lambda (x) (eq x 'c)) '(a b c)) 2)))
(define-test sacla-must-sequence.944 (:tag :sacla)
 (assert-true (null (position-if #'(lambda (arg) (eq arg 'x)) '(a b c)))))
(define-test sacla-must-sequence.945 (:tag :sacla)
 (assert-true (null (position-if #'(lambda (x) (eq x 'a)) '(a b c) :start 1))))
(define-test sacla-must-sequence.946 (:tag :sacla)
 (assert-true (null (position-if #'(lambda (x) (eq x 'b)) '(a b c) :start 2))))
(define-test sacla-must-sequence.947 (:tag :sacla)
 (assert-true (null (position-if #'(lambda (x) (eq x 'c)) '(a b c) :start 3))))
(define-test sacla-must-sequence.948 (:tag :sacla)
 (assert-true
  (null (position-if #'(lambda (x) (eq x 'a)) '(a b c) :start 0 :end 0))))
(define-test sacla-must-sequence.949 (:tag :sacla)
 (assert-true
  (null
   (position-if #'(lambda (x) (eq x 'a))
                '(a b c)
                :start 0
                :end 0
                :from-end t))))
(define-test sacla-must-sequence.950 (:tag :sacla)
 (assert-true
  (null (position-if #'(lambda (x) (eq x 'a)) '(a b c) :start 1 :end 1))))
(define-test sacla-must-sequence.951 (:tag :sacla)
 (assert-true
  (null
   (position-if #'(lambda (x) (eq x 'a))
                '(a b c)
                :start 1
                :end 1
                :from-end t))))
(define-test sacla-must-sequence.952 (:tag :sacla)
 (assert-true
  (null (position-if #'(lambda (x) (eq x 'a)) '(a b c) :start 2 :end 2))))
(define-test sacla-must-sequence.953 (:tag :sacla)
 (assert-true
  (null
   (position-if #'(lambda (x) (eq x 'a))
                '(a b c)
                :start 2
                :end 2
                :from-end t))))
(define-test sacla-must-sequence.954 (:tag :sacla)
 (assert-true
  (null (position-if #'(lambda (x) (eq x 'a)) '(a b c) :start 3 :end 3))))
(define-test sacla-must-sequence.955 (:tag :sacla)
 (assert-true
  (null
   (position-if #'(lambda (x) (eq x 'a))
                '(a b c)
                :start 3
                :end 3
                :from-end t))))
(define-test sacla-must-sequence.956 (:tag :sacla)
 (assert-true (eql (position-if #'(lambda (x) (eq x 'a)) '(a b c) :end nil) 0)))
(define-test sacla-must-sequence.957 (:tag :sacla)
 (assert-true (eql (position-if #'(lambda (x) (eq x 'b)) '(a b c) :end nil) 1)))
(define-test sacla-must-sequence.958 (:tag :sacla)
 (assert-true (eql (position-if #'(lambda (x) (eq x 'c)) '(a b c) :end nil) 2)))
(define-test sacla-must-sequence.959 (:tag :sacla)
 (assert-true (eql (position-if #'(lambda (x) (eq x 'a)) '(a b c) :end 1) 0)))
(define-test sacla-must-sequence.960 (:tag :sacla)
 (assert-true (eql (position-if #'(lambda (x) (eq x 'b)) '(a b c) :end 2) 1)))
(define-test sacla-must-sequence.961 (:tag :sacla)
 (assert-true (eql (position-if #'(lambda (x) (eq x 'c)) '(a b c) :end 3) 2)))
(define-test sacla-must-sequence.962 (:tag :sacla)
 (assert-true (null (position-if #'(lambda (x) (eq x 'a)) '(a b c) :end 0))))
(define-test sacla-must-sequence.963 (:tag :sacla)
 (assert-true (null (position-if #'(lambda (x) (eq x 'b)) '(a b c) :end 1))))
(define-test sacla-must-sequence.964 (:tag :sacla)
 (assert-true (null (position-if #'(lambda (x) (eq x 'c)) '(a b c) :end 2))))
(define-test sacla-must-sequence.965 (:tag :sacla)
 (assert-true (null (position-if #'(lambda (x) (eq x 'a)) '((a) (b) (c))))))
(define-test sacla-must-sequence.966 (:tag :sacla)
 (assert-true
  (eql (position-if #'(lambda (x) (eq x 'a)) '((a) (b) (c)) :key #'car) 0)))
(define-test sacla-must-sequence.967 (:tag :sacla)
 (assert-true
  (eql (position-if #'(lambda (x) (eq x 'b)) '((a) (b) (c)) :key #'car) 1)))
(define-test sacla-must-sequence.968 (:tag :sacla)
 (assert-true
  (eql (position-if #'(lambda (x) (eq x 'c)) '((a) (b) (c)) :key #'car) 2)))
(define-test sacla-must-sequence.969 (:tag :sacla)
 (assert-true
  (null (position-if #'(lambda (x) (eq x 'z)) '((a) (b) (c)) :key #'car))))
(define-test sacla-must-sequence.970 (:tag :sacla)
 (assert-true
  (eql
   (position-if #'(lambda (x) (eq x 'a))
                '((a) (b) (c) (a a) (b b) (c c))
                :key #'car)
   0)))
(define-test sacla-must-sequence.971 (:tag :sacla)
 (assert-true
  (eql
   (position-if #'(lambda (x) (eq x 'a))
                '((a) (b) (c) (a a) (b b) (c c))
                :key #'car
                :from-end t)
   3)))
(define-test sacla-must-sequence.972 (:tag :sacla)
 (assert-true
  (eql
   (position-if #'(lambda (x) (eq x 'b))
                '((a) (b) (c) (a a) (b b) (c c))
                :key #'car)
   1)))
(define-test sacla-must-sequence.973 (:tag :sacla)
 (assert-true
  (eql
   (position-if #'(lambda (x) (eq x 'b))
                '((a) (b) (c) (a a) (b b) (c c))
                :key #'car
                :from-end t)
   4)))
(define-test sacla-must-sequence.974 (:tag :sacla)
 (assert-true
  (eql
   (position-if #'(lambda (x) (eq x 'c))
                '((a) (b) (c) (a a) (b b) (c c))
                :key #'car)
   2)))
(define-test sacla-must-sequence.975 (:tag :sacla)
 (assert-true
  (eql
   (position-if #'(lambda (x) (eq x 'c))
                '((a) (b) (c) (a a) (b b) (c c))
                :key #'car
                :from-end t)
   5)))
(define-test sacla-must-sequence.976 (:tag :sacla)
 (assert-true
  (null
   (position-if #'(lambda (x) (eq x 'z))
                '((a) (b) (c) (a a) (b b) (c c))
                :key #'car))))
(define-test sacla-must-sequence.977 (:tag :sacla)
 (assert-true
  (null
   (position-if #'(lambda (x) (eq x 'z))
                '((a) (b) (c) (a a) (b b) (c c))
                :key #'car
                :from-end t))))
(define-test sacla-must-sequence.978 (:tag :sacla)
 (assert-true
  (eql
   (position-if #'(lambda (x) (eq x 'a))
                '((a) (b) (c) (a a) (b b) (c c) (a a a))
                :key #'car
                :from-end t)
   6)))
(define-test sacla-must-sequence.979 (:tag :sacla)
 (assert-true
  (eql
   (position-if #'(lambda (x) (eq x 'a))
                '((a) (b) (c) (a a) (b b) (c c) (a a a))
                :key #'car
                :from-end t
                :end nil)
   6)))
(define-test sacla-must-sequence.980 (:tag :sacla)
 (assert-true
  (eql
   (position-if #'(lambda (x) (eq x 'a))
                '((a) (b) (c) (a a) (b b) (c c) (a a a))
                :key #'car
                :from-end t
                :end 6)
   3)))
(define-test sacla-must-sequence.981 (:tag :sacla)
 (assert-true
  (null
   (position-if #'(lambda (x) (eq x 'a))
                '((a) (b) (c) (a a) (b b) (c c) (a a a))
                :key #'car
                :from-end t
                :start 1
                :end 3))))
(define-test sacla-must-sequence.982 (:tag :sacla)
 (assert-true
  (null (position-if #'(lambda (x) (eql x #\c)) '("abc" "bcd" "cde")))))
(define-test sacla-must-sequence.983 (:tag :sacla)
 (assert-true
  (eql
   (position-if #'(lambda (x) (eql x #\c))
                '("abc" "bcd" "cde")
                :key #'(lambda (arg) (char arg 0)))
   2)))
(define-test sacla-must-sequence.984 (:tag :sacla)
 (assert-true
  (eql
   (position-if #'(lambda (x) (char> #\c x))
                '("abc" "bcd" "cde")
                :key #'(lambda (arg) (char arg 0)))
   0)))
(define-test sacla-must-sequence.985 (:tag :sacla)
 (assert-true
  (eql
   (position-if #'(lambda (x) (char> #\c x))
                '("abc" "bcd" "cde")
                :start 1
                :key #'(lambda (arg) (char arg 0)))
   1)))
(define-test sacla-must-sequence.986 (:tag :sacla)
 (assert-true (eql (position-if #'(lambda (x) (eq x 'a)) #(a b c)) 0)))
(define-test sacla-must-sequence.987 (:tag :sacla)
 (assert-true (eql (position-if #'(lambda (x) (eq x 'b)) #(a b c)) 1)))
(define-test sacla-must-sequence.988 (:tag :sacla)
 (assert-true (eql (position-if #'(lambda (x) (eq x 'c)) #(a b c)) 2)))
(define-test sacla-must-sequence.989 (:tag :sacla)
 (assert-true (null (position-if #'(lambda (arg) (eq arg 'x)) #(a b c)))))
(define-test sacla-must-sequence.990 (:tag :sacla)
 (assert-true (null (position-if #'(lambda (x) (eq x 'a)) #(a b c) :start 1))))
(define-test sacla-must-sequence.991 (:tag :sacla)
 (assert-true (null (position-if #'(lambda (x) (eq x 'b)) #(a b c) :start 2))))
(define-test sacla-must-sequence.992 (:tag :sacla)
 (assert-true (null (position-if #'(lambda (x) (eq x 'c)) #(a b c) :start 3))))
(define-test sacla-must-sequence.993 (:tag :sacla)
 (assert-true
  (null (position-if #'(lambda (x) (eq x 'a)) #(a b c) :start 0 :end 0))))
(define-test sacla-must-sequence.994 (:tag :sacla)
 (assert-true
  (null
   (position-if #'(lambda (x) (eq x 'a))
                #(a b c)
                :start 0
                :end 0
                :from-end t))))
(define-test sacla-must-sequence.995 (:tag :sacla)
 (assert-true
  (null (position-if #'(lambda (x) (eq x 'a)) #(a b c) :start 1 :end 1))))
(define-test sacla-must-sequence.996 (:tag :sacla)
 (assert-true
  (null
   (position-if #'(lambda (x) (eq x 'a))
                #(a b c)
                :start 1
                :end 1
                :from-end t))))
(define-test sacla-must-sequence.997 (:tag :sacla)
 (assert-true
  (null (position-if #'(lambda (x) (eq x 'a)) #(a b c) :start 2 :end 2))))
(define-test sacla-must-sequence.998 (:tag :sacla)
 (assert-true
  (null
   (position-if #'(lambda (x) (eq x 'a))
                #(a b c)
                :start 2
                :end 2
                :from-end t))))
(define-test sacla-must-sequence.999 (:tag :sacla)
 (assert-true
  (null (position-if #'(lambda (x) (eq x 'a)) #(a b c) :start 3 :end 3))))
(define-test sacla-must-sequence.1000 (:tag :sacla)
 (assert-true
  (null
   (position-if #'(lambda (x) (eq x 'a))
                #(a b c)
                :start 3
                :end 3
                :from-end t))))
(define-test sacla-must-sequence.1001 (:tag :sacla)
 (assert-true (eql (position-if #'(lambda (x) (eq x 'a)) #(a b c) :end nil) 0)))
(define-test sacla-must-sequence.1002 (:tag :sacla)
 (assert-true (eql (position-if #'(lambda (x) (eq x 'b)) #(a b c) :end nil) 1)))
(define-test sacla-must-sequence.1003 (:tag :sacla)
 (assert-true (eql (position-if #'(lambda (x) (eq x 'c)) #(a b c) :end nil) 2)))
(define-test sacla-must-sequence.1004 (:tag :sacla)
 (assert-true (eql (position-if #'(lambda (x) (eq x 'a)) #(a b c) :end 1) 0)))
(define-test sacla-must-sequence.1005 (:tag :sacla)
 (assert-true (eql (position-if #'(lambda (x) (eq x 'b)) #(a b c) :end 2) 1)))
(define-test sacla-must-sequence.1006 (:tag :sacla)
 (assert-true (eql (position-if #'(lambda (x) (eq x 'c)) #(a b c) :end 3) 2)))
(define-test sacla-must-sequence.1007 (:tag :sacla)
 (assert-true (null (position-if #'(lambda (x) (eq x 'a)) #(a b c) :end 0))))
(define-test sacla-must-sequence.1008 (:tag :sacla)
 (assert-true (null (position-if #'(lambda (x) (eq x 'b)) #(a b c) :end 1))))
(define-test sacla-must-sequence.1009 (:tag :sacla)
 (assert-true (null (position-if #'(lambda (x) (eq x 'c)) #(a b c) :end 2))))
(define-test sacla-must-sequence.1010 (:tag :sacla)
 (assert-true (null (position-if #'(lambda (x) (eq x 'a)) #((a) (b) (c))))))
(define-test sacla-must-sequence.1011 (:tag :sacla)
 (assert-true
  (eql (position-if #'(lambda (x) (eq x 'a)) #((a) (b) (c)) :key #'car) 0)))
(define-test sacla-must-sequence.1012 (:tag :sacla)
 (assert-true
  (eql (position-if #'(lambda (x) (eq x 'b)) #((a) (b) (c)) :key #'car) 1)))
(define-test sacla-must-sequence.1013 (:tag :sacla)
 (assert-true
  (eql (position-if #'(lambda (x) (eq x 'c)) #((a) (b) (c)) :key #'car) 2)))
(define-test sacla-must-sequence.1014 (:tag :sacla)
 (assert-true
  (null (position-if #'(lambda (x) (eq x 'z)) #((a) (b) (c)) :key #'car))))
(define-test sacla-must-sequence.1015 (:tag :sacla)
 (assert-true
  (eql
   (position-if #'(lambda (x) (eq x 'a))
                #((a) (b) (c) (a a) (b b) (c c))
                :key #'car)
   0)))
(define-test sacla-must-sequence.1016 (:tag :sacla)
 (assert-true
  (eql
   (position-if #'(lambda (x) (eq x 'a))
                #((a) (b) (c) (a a) (b b) (c c))
                :key #'car
                :from-end t)
   3)))
(define-test sacla-must-sequence.1017 (:tag :sacla)
 (assert-true
  (eql
   (position-if #'(lambda (x) (eq x 'b))
                #((a) (b) (c) (a a) (b b) (c c))
                :key #'car)
   1)))
(define-test sacla-must-sequence.1018 (:tag :sacla)
 (assert-true
  (eql
   (position-if #'(lambda (x) (eq x 'b))
                #((a) (b) (c) (a a) (b b) (c c))
                :key #'car
                :from-end t)
   4)))
(define-test sacla-must-sequence.1019 (:tag :sacla)
 (assert-true
  (eql
   (position-if #'(lambda (x) (eq x 'c))
                #((a) (b) (c) (a a) (b b) (c c))
                :key #'car)
   2)))
(define-test sacla-must-sequence.1020 (:tag :sacla)
 (assert-true
  (eql
   (position-if #'(lambda (x) (eq x 'c))
                #((a) (b) (c) (a a) (b b) (c c))
                :key #'car
                :from-end t)
   5)))
(define-test sacla-must-sequence.1021 (:tag :sacla)
 (assert-true
  (null
   (position-if #'(lambda (x) (eq x 'z))
                #((a) (b) (c) (a a) (b b) (c c))
                :key #'car))))
(define-test sacla-must-sequence.1022 (:tag :sacla)
 (assert-true
  (null
   (position-if #'(lambda (x) (eq x 'z))
                #((a) (b) (c) (a a) (b b) (c c))
                :key #'car
                :from-end t))))
(define-test sacla-must-sequence.1023 (:tag :sacla)
 (assert-true
  (eql
   (position-if #'(lambda (x) (eq x 'a))
                #((a) (b) (c) (a a) (b b) (c c) (a a a))
                :key #'car
                :from-end t)
   6)))
(define-test sacla-must-sequence.1024 (:tag :sacla)
 (assert-true
  (eql
   (position-if #'(lambda (x) (eq x 'a))
                #((a) (b) (c) (a a) (b b) (c c) (a a a))
                :key #'car
                :from-end t
                :end nil)
   6)))
(define-test sacla-must-sequence.1025 (:tag :sacla)
 (assert-true
  (eql
   (position-if #'(lambda (x) (eq x 'a))
                #((a) (b) (c) (a a) (b b) (c c) (a a a))
                :key #'car
                :from-end t
                :end 6)
   3)))
(define-test sacla-must-sequence.1026 (:tag :sacla)
 (assert-true
  (null
   (position-if #'(lambda (x) (eq x 'a))
                #((a) (b) (c) (a a) (b b) (c c) (a a a))
                :key #'car
                :from-end t
                :start 1
                :end 3))))
(define-test sacla-must-sequence.1027 (:tag :sacla)
 (assert-true
  (null (position-if #'(lambda (x) (eql x #\c)) #("abc" "bcd" "cde")))))
(define-test sacla-must-sequence.1028 (:tag :sacla)
 (assert-true
  (eql
   (position-if #'(lambda (x) (eql x #\c))
                #("abc" "bcd" "cde")
                :key #'(lambda (arg) (char arg 0)))
   2)))
(define-test sacla-must-sequence.1029 (:tag :sacla)
 (assert-true
  (eql
   (position-if #'(lambda (x) (char> #\c x))
                #("abc" "bcd" "cde")
                :key #'(lambda (arg) (char arg 0)))
   0)))
(define-test sacla-must-sequence.1030 (:tag :sacla)
 (assert-true
  (eql
   (position-if #'(lambda (x) (char> #\c x))
                #("abc" "bcd" "cde")
                :start 1
                :key #'(lambda (arg) (char arg 0)))
   1)))
(define-test sacla-must-sequence.1031 (:tag :sacla)
 (assert-true
  (eql (position-if-not #'(lambda (x) (not (eq x 'a))) '(a b c)) 0)))
(define-test sacla-must-sequence.1032 (:tag :sacla)
 (assert-true
  (eql (position-if-not #'(lambda (x) (not (eq x 'b))) '(a b c)) 1)))
(define-test sacla-must-sequence.1033 (:tag :sacla)
 (assert-true
  (eql (position-if-not #'(lambda (x) (not (eq x 'c))) '(a b c)) 2)))
(define-test sacla-must-sequence.1034 (:tag :sacla)
 (assert-true
  (null (position-if-not #'(lambda (arg) (not (eq arg 'x))) '(a b c)))))
(define-test sacla-must-sequence.1035 (:tag :sacla)
 (assert-true
  (null (position-if-not #'(lambda (x) (not (eq x 'a))) '(a b c) :start 1))))
(define-test sacla-must-sequence.1036 (:tag :sacla)
 (assert-true
  (null (position-if-not #'(lambda (x) (not (eq x 'b))) '(a b c) :start 2))))
(define-test sacla-must-sequence.1037 (:tag :sacla)
 (assert-true
  (null (position-if-not #'(lambda (x) (not (eq x 'c))) '(a b c) :start 3))))
(define-test sacla-must-sequence.1038 (:tag :sacla)
 (assert-true
  (null
   (position-if-not #'(lambda (x) (not (eq x 'a))) '(a b c) :start 0 :end 0))))
(define-test sacla-must-sequence.1039 (:tag :sacla)
 (assert-true
  (null
   (position-if-not #'(lambda (x) (not (eq x 'a)))
                    '(a b c)
                    :start 0
                    :end 0
                    :from-end t))))
(define-test sacla-must-sequence.1040 (:tag :sacla)
 (assert-true
  (null
   (position-if-not #'(lambda (x) (not (eq x 'a))) '(a b c) :start 1 :end 1))))
(define-test sacla-must-sequence.1041 (:tag :sacla)
 (assert-true
  (null
   (position-if-not #'(lambda (x) (not (eq x 'a)))
                    '(a b c)
                    :start 1
                    :end 1
                    :from-end t))))
(define-test sacla-must-sequence.1042 (:tag :sacla)
 (assert-true
  (null
   (position-if-not #'(lambda (x) (not (eq x 'a))) '(a b c) :start 2 :end 2))))
(define-test sacla-must-sequence.1043 (:tag :sacla)
 (assert-true
  (null
   (position-if-not #'(lambda (x) (not (eq x 'a)))
                    '(a b c)
                    :start 2
                    :end 2
                    :from-end t))))
(define-test sacla-must-sequence.1044 (:tag :sacla)
 (assert-true
  (null
   (position-if-not #'(lambda (x) (not (eq x 'a))) '(a b c) :start 3 :end 3))))
(define-test sacla-must-sequence.1045 (:tag :sacla)
 (assert-true
  (null
   (position-if-not #'(lambda (x) (not (eq x 'a)))
                    '(a b c)
                    :start 3
                    :end 3
                    :from-end t))))
(define-test sacla-must-sequence.1046 (:tag :sacla)
 (assert-true
  (eql (position-if-not #'(lambda (x) (not (eq x 'a))) '(a b c) :end nil) 0)))
(define-test sacla-must-sequence.1047 (:tag :sacla)
 (assert-true
  (eql (position-if-not #'(lambda (x) (not (eq x 'b))) '(a b c) :end nil) 1)))
(define-test sacla-must-sequence.1048 (:tag :sacla)
 (assert-true
  (eql (position-if-not #'(lambda (x) (not (eq x 'c))) '(a b c) :end nil) 2)))
(define-test sacla-must-sequence.1049 (:tag :sacla)
 (assert-true
  (eql (position-if-not #'(lambda (x) (not (eq x 'a))) '(a b c) :end 1) 0)))
(define-test sacla-must-sequence.1050 (:tag :sacla)
 (assert-true
  (eql (position-if-not #'(lambda (x) (not (eq x 'b))) '(a b c) :end 2) 1)))
(define-test sacla-must-sequence.1051 (:tag :sacla)
 (assert-true
  (eql (position-if-not #'(lambda (x) (not (eq x 'c))) '(a b c) :end 3) 2)))
(define-test sacla-must-sequence.1052 (:tag :sacla)
 (assert-true
  (null (position-if-not #'(lambda (x) (not (eq x 'a))) '(a b c) :end 0))))
(define-test sacla-must-sequence.1053 (:tag :sacla)
 (assert-true
  (null (position-if-not #'(lambda (x) (not (eq x 'b))) '(a b c) :end 1))))
(define-test sacla-must-sequence.1054 (:tag :sacla)
 (assert-true
  (null (position-if-not #'(lambda (x) (not (eq x 'c))) '(a b c) :end 2))))
(define-test sacla-must-sequence.1055 (:tag :sacla)
 (assert-true
  (null (position-if-not #'(lambda (x) (not (eq x 'a))) '((a) (b) (c))))))
(define-test sacla-must-sequence.1056 (:tag :sacla)
 (assert-true
  (eql
   (position-if-not #'(lambda (x) (not (eq x 'a))) '((a) (b) (c)) :key #'car)
   0)))
(define-test sacla-must-sequence.1057 (:tag :sacla)
 (assert-true
  (eql
   (position-if-not #'(lambda (x) (not (eq x 'b))) '((a) (b) (c)) :key #'car)
   1)))
(define-test sacla-must-sequence.1058 (:tag :sacla)
 (assert-true
  (eql
   (position-if-not #'(lambda (x) (not (eq x 'c))) '((a) (b) (c)) :key #'car)
   2)))
(define-test sacla-must-sequence.1059 (:tag :sacla)
 (assert-true
  (null
   (position-if-not #'(lambda (x) (not (eq x 'z))) '((a) (b) (c)) :key #'car))))
(define-test sacla-must-sequence.1060 (:tag :sacla)
 (assert-true
  (eql
   (position-if-not #'(lambda (x) (not (eq x 'a)))
                    '((a) (b) (c) (a a) (b b) (c c))
                    :key #'car)
   0)))
(define-test sacla-must-sequence.1061 (:tag :sacla)
 (assert-true
  (eql
   (position-if-not #'(lambda (x) (not (eq x 'a)))
                    '((a) (b) (c) (a a) (b b) (c c))
                    :key #'car
                    :from-end t)
   3)))
(define-test sacla-must-sequence.1062 (:tag :sacla)
 (assert-true
  (eql
   (position-if-not #'(lambda (x) (not (eq x 'b)))
                    '((a) (b) (c) (a a) (b b) (c c))
                    :key #'car)
   1)))
(define-test sacla-must-sequence.1063 (:tag :sacla)
 (assert-true
  (eql
   (position-if-not #'(lambda (x) (not (eq x 'b)))
                    '((a) (b) (c) (a a) (b b) (c c))
                    :key #'car
                    :from-end t)
   4)))
(define-test sacla-must-sequence.1064 (:tag :sacla)
 (assert-true
  (eql
   (position-if-not #'(lambda (x) (not (eq x 'c)))
                    '((a) (b) (c) (a a) (b b) (c c))
                    :key #'car)
   2)))
(define-test sacla-must-sequence.1065 (:tag :sacla)
 (assert-true
  (eql
   (position-if-not #'(lambda (x) (not (eq x 'c)))
                    '((a) (b) (c) (a a) (b b) (c c))
                    :key #'car
                    :from-end t)
   5)))
(define-test sacla-must-sequence.1066 (:tag :sacla)
 (assert-true
  (null
   (position-if-not #'(lambda (x) (not (eq x 'z)))
                    '((a) (b) (c) (a a) (b b) (c c))
                    :key #'car))))
(define-test sacla-must-sequence.1067 (:tag :sacla)
 (assert-true
  (null
   (position-if-not #'(lambda (x) (not (eq x 'z)))
                    '((a) (b) (c) (a a) (b b) (c c))
                    :key #'car
                    :from-end t))))
(define-test sacla-must-sequence.1068 (:tag :sacla)
 (assert-true
  (eql
   (position-if-not #'(lambda (x) (not (eq x 'a)))
                    '((a) (b) (c) (a a) (b b) (c c) (a a a))
                    :key #'car
                    :from-end t)
   6)))
(define-test sacla-must-sequence.1069 (:tag :sacla)
 (assert-true
  (eql
   (position-if-not #'(lambda (x) (not (eq x 'a)))
                    '((a) (b) (c) (a a) (b b) (c c) (a a a))
                    :key #'car
                    :from-end t
                    :end nil)
   6)))
(define-test sacla-must-sequence.1070 (:tag :sacla)
 (assert-true
  (eql
   (position-if-not #'(lambda (x) (not (eq x 'a)))
                    '((a) (b) (c) (a a) (b b) (c c) (a a a))
                    :key #'car
                    :from-end t
                    :end 6)
   3)))
(define-test sacla-must-sequence.1071 (:tag :sacla)
 (assert-true
  (null
   (position-if-not #'(lambda (x) (not (eq x 'a)))
                    '((a) (b) (c) (a a) (b b) (c c) (a a a))
                    :key #'car
                    :from-end t
                    :start 1
                    :end 3))))
(define-test sacla-must-sequence.1072 (:tag :sacla)
 (assert-true
  (null
   (position-if-not #'(lambda (x) (not (eql x #\c))) '("abc" "bcd" "cde")))))
(define-test sacla-must-sequence.1073 (:tag :sacla)
 (assert-true
  (eql
   (position-if-not #'(lambda (x) (not (eql x #\c)))
                    '("abc" "bcd" "cde")
                    :key #'(lambda (arg) (char arg 0)))
   2)))
(define-test sacla-must-sequence.1074 (:tag :sacla)
 (assert-true
  (eql
   (position-if-not #'(lambda (x) (not (char> #\c x)))
                    '("abc" "bcd" "cde")
                    :key #'(lambda (arg) (char arg 0)))
   0)))
(define-test sacla-must-sequence.1075 (:tag :sacla)
 (assert-true
  (eql
   (position-if-not #'(lambda (x) (not (char> #\c x)))
                    '("abc" "bcd" "cde")
                    :start 1
                    :key #'(lambda (arg) (char arg 0)))
   1)))
(define-test sacla-must-sequence.1076 (:tag :sacla)
 (assert-true
  (eql (position-if-not #'(lambda (x) (not (eq x 'a))) #(a b c)) 0)))
(define-test sacla-must-sequence.1077 (:tag :sacla)
 (assert-true
  (eql (position-if-not #'(lambda (x) (not (eq x 'b))) #(a b c)) 1)))
(define-test sacla-must-sequence.1078 (:tag :sacla)
 (assert-true
  (eql (position-if-not #'(lambda (x) (not (eq x 'c))) #(a b c)) 2)))
(define-test sacla-must-sequence.1079 (:tag :sacla)
 (assert-true
  (null (position-if-not #'(lambda (arg) (not (eq arg 'x))) #(a b c)))))
(define-test sacla-must-sequence.1080 (:tag :sacla)
 (assert-true
  (null (position-if-not #'(lambda (x) (not (eq x 'a))) #(a b c) :start 1))))
(define-test sacla-must-sequence.1081 (:tag :sacla)
 (assert-true
  (null (position-if-not #'(lambda (x) (not (eq x 'b))) #(a b c) :start 2))))
(define-test sacla-must-sequence.1082 (:tag :sacla)
 (assert-true
  (null (position-if-not #'(lambda (x) (not (eq x 'c))) #(a b c) :start 3))))
(define-test sacla-must-sequence.1083 (:tag :sacla)
 (assert-true
  (null
   (position-if-not #'(lambda (x) (not (eq x 'a))) #(a b c) :start 0 :end 0))))
(define-test sacla-must-sequence.1084 (:tag :sacla)
 (assert-true
  (null
   (position-if-not #'(lambda (x) (not (eq x 'a)))
                    #(a b c)
                    :start 0
                    :end 0
                    :from-end t))))
(define-test sacla-must-sequence.1085 (:tag :sacla)
 (assert-true
  (null
   (position-if-not #'(lambda (x) (not (eq x 'a))) #(a b c) :start 1 :end 1))))
(define-test sacla-must-sequence.1086 (:tag :sacla)
 (assert-true
  (null
   (position-if-not #'(lambda (x) (not (eq x 'a)))
                    #(a b c)
                    :start 1
                    :end 1
                    :from-end t))))
(define-test sacla-must-sequence.1087 (:tag :sacla)
 (assert-true
  (null
   (position-if-not #'(lambda (x) (not (eq x 'a))) #(a b c) :start 2 :end 2))))
(define-test sacla-must-sequence.1088 (:tag :sacla)
 (assert-true
  (null
   (position-if-not #'(lambda (x) (not (eq x 'a)))
                    #(a b c)
                    :start 2
                    :end 2
                    :from-end t))))
(define-test sacla-must-sequence.1089 (:tag :sacla)
 (assert-true
  (null
   (position-if-not #'(lambda (x) (not (eq x 'a))) #(a b c) :start 3 :end 3))))
(define-test sacla-must-sequence.1090 (:tag :sacla)
 (assert-true
  (null
   (position-if-not #'(lambda (x) (not (eq x 'a)))
                    #(a b c)
                    :start 3
                    :end 3
                    :from-end t))))
(define-test sacla-must-sequence.1091 (:tag :sacla)
 (assert-true
  (eql (position-if-not #'(lambda (x) (not (eq x 'a))) #(a b c) :end nil) 0)))
(define-test sacla-must-sequence.1092 (:tag :sacla)
 (assert-true
  (eql (position-if-not #'(lambda (x) (not (eq x 'b))) #(a b c) :end nil) 1)))
(define-test sacla-must-sequence.1093 (:tag :sacla)
 (assert-true
  (eql (position-if-not #'(lambda (x) (not (eq x 'c))) #(a b c) :end nil) 2)))
(define-test sacla-must-sequence.1094 (:tag :sacla)
 (assert-true
  (eql (position-if-not #'(lambda (x) (not (eq x 'a))) #(a b c) :end 1) 0)))
(define-test sacla-must-sequence.1095 (:tag :sacla)
 (assert-true
  (eql (position-if-not #'(lambda (x) (not (eq x 'b))) #(a b c) :end 2) 1)))
(define-test sacla-must-sequence.1096 (:tag :sacla)
 (assert-true
  (eql (position-if-not #'(lambda (x) (not (eq x 'c))) #(a b c) :end 3) 2)))
(define-test sacla-must-sequence.1097 (:tag :sacla)
 (assert-true
  (null (position-if-not #'(lambda (x) (not (eq x 'a))) #(a b c) :end 0))))
(define-test sacla-must-sequence.1098 (:tag :sacla)
 (assert-true
  (null (position-if-not #'(lambda (x) (not (eq x 'b))) #(a b c) :end 1))))
(define-test sacla-must-sequence.1099 (:tag :sacla)
 (assert-true
  (null (position-if-not #'(lambda (x) (not (eq x 'c))) #(a b c) :end 2))))
(define-test sacla-must-sequence.1100 (:tag :sacla)
 (assert-true
  (null (position-if-not #'(lambda (x) (not (eq x 'a))) #((a) (b) (c))))))
(define-test sacla-must-sequence.1101 (:tag :sacla)
 (assert-true
  (eql
   (position-if-not #'(lambda (x) (not (eq x 'a))) #((a) (b) (c)) :key #'car)
   0)))
(define-test sacla-must-sequence.1102 (:tag :sacla)
 (assert-true
  (eql
   (position-if-not #'(lambda (x) (not (eq x 'b))) #((a) (b) (c)) :key #'car)
   1)))
(define-test sacla-must-sequence.1103 (:tag :sacla)
 (assert-true
  (eql
   (position-if-not #'(lambda (x) (not (eq x 'c))) #((a) (b) (c)) :key #'car)
   2)))
(define-test sacla-must-sequence.1104 (:tag :sacla)
 (assert-true
  (null
   (position-if-not #'(lambda (x) (not (eq x 'z))) #((a) (b) (c)) :key #'car))))
(define-test sacla-must-sequence.1105 (:tag :sacla)
 (assert-true
  (eql
   (position-if-not #'(lambda (x) (not (eq x 'a)))
                    #((a) (b) (c) (a a) (b b) (c c))
                    :key #'car)
   0)))
(define-test sacla-must-sequence.1106 (:tag :sacla)
 (assert-true
  (eql
   (position-if-not #'(lambda (x) (not (eq x 'a)))
                    #((a) (b) (c) (a a) (b b) (c c))
                    :key #'car
                    :from-end t)
   3)))
(define-test sacla-must-sequence.1107 (:tag :sacla)
 (assert-true
  (eql
   (position-if-not #'(lambda (x) (not (eq x 'b)))
                    #((a) (b) (c) (a a) (b b) (c c))
                    :key #'car)
   1)))
(define-test sacla-must-sequence.1108 (:tag :sacla)
 (assert-true
  (eql
   (position-if-not #'(lambda (x) (not (eq x 'b)))
                    #((a) (b) (c) (a a) (b b) (c c))
                    :key #'car
                    :from-end t)
   4)))
(define-test sacla-must-sequence.1109 (:tag :sacla)
 (assert-true
  (eql
   (position-if-not #'(lambda (x) (not (eq x 'c)))
                    #((a) (b) (c) (a a) (b b) (c c))
                    :key #'car)
   2)))
(define-test sacla-must-sequence.1110 (:tag :sacla)
 (assert-true
  (eql
   (position-if-not #'(lambda (x) (not (eq x 'c)))
                    #((a) (b) (c) (a a) (b b) (c c))
                    :key #'car
                    :from-end t)
   5)))
(define-test sacla-must-sequence.1111 (:tag :sacla)
 (assert-true
  (null
   (position-if-not #'(lambda (x) (not (eq x 'z)))
                    #((a) (b) (c) (a a) (b b) (c c))
                    :key #'car))))
(define-test sacla-must-sequence.1112 (:tag :sacla)
 (assert-true
  (null
   (position-if-not #'(lambda (x) (not (eq x 'z)))
                    #((a) (b) (c) (a a) (b b) (c c))
                    :key #'car
                    :from-end t))))
(define-test sacla-must-sequence.1113 (:tag :sacla)
 (assert-true
  (eql
   (position-if-not #'(lambda (x) (not (eq x 'a)))
                    #((a) (b) (c) (a a) (b b) (c c) (a a a))
                    :key #'car
                    :from-end t)
   6)))
(define-test sacla-must-sequence.1114 (:tag :sacla)
 (assert-true
  (eql
   (position-if-not #'(lambda (x) (not (eq x 'a)))
                    #((a) (b) (c) (a a) (b b) (c c) (a a a))
                    :key #'car
                    :from-end t
                    :end nil)
   6)))
(define-test sacla-must-sequence.1115 (:tag :sacla)
 (assert-true
  (eql
   (position-if-not #'(lambda (x) (not (eq x 'a)))
                    #((a) (b) (c) (a a) (b b) (c c) (a a a))
                    :key #'car
                    :from-end t
                    :end 6)
   3)))
(define-test sacla-must-sequence.1116 (:tag :sacla)
 (assert-true
  (null
   (position-if-not #'(lambda (x) (not (eq x 'a)))
                    #((a) (b) (c) (a a) (b b) (c c) (a a a))
                    :key #'car
                    :from-end t
                    :start 1
                    :end 3))))
(define-test sacla-must-sequence.1117 (:tag :sacla)
 (assert-true
  (eql
   (position-if-not #'(lambda (x) (not (eql x #\c)))
                    #("abc" "bcd" "cde")
                    :key #'(lambda (arg) (char arg 0)))
   2)))
(define-test sacla-must-sequence.1118 (:tag :sacla)
 (assert-true
  (eql
   (position-if-not #'(lambda (x) (not (char> #\c x)))
                    #("abc" "bcd" "cde")
                    :key #'(lambda (arg) (char arg 0)))
   0)))
(define-test sacla-must-sequence.1119 (:tag :sacla)
 (assert-true
  (eql
   (position-if-not #'(lambda (x) (not (char> #\c x)))
                    #("abc" "bcd" "cde")
                    :start 1
                    :key #'(lambda (arg) (char arg 0)))
   1)))
(define-test sacla-must-sequence.1120 (:tag :sacla)
 (assert-true (eql (search "dog" "it's a dog's life") 7)))
(define-test sacla-must-sequence.1121 (:tag :sacla)
 (assert-true (eql (search '(0 1) '(2 4 6 1 3 5) :key #'oddp) 2)))
(define-test sacla-must-sequence.1122 (:tag :sacla)
 (assert-true (eql (search 'nil 'nil) 0)))
(define-test sacla-must-sequence.1123 (:tag :sacla)
 (assert-true (null (search '(a b c) '(x y z)))))
(define-test sacla-must-sequence.1124 (:tag :sacla)
 (assert-true (eql (search 'nil '(x y z)) 0)))
(define-test sacla-must-sequence.1125 (:tag :sacla)
 (assert-true (eql (search '(a) '(a)) 0)))
(define-test sacla-must-sequence.1126 (:tag :sacla)
 (assert-true (eql (search '(a b c) '(a b c x y z)) 0)))
(define-test sacla-must-sequence.1127 (:tag :sacla)
 (assert-true (eql (search '(a b c) '(x a b c y z)) 1)))
(define-test sacla-must-sequence.1128 (:tag :sacla)
 (assert-true (eql (search '(a b c) '(x y a b c z)) 2)))
(define-test sacla-must-sequence.1129 (:tag :sacla)
 (assert-true (eql (search '(a b c) '(x y z a b c)) 3)))
(define-test sacla-must-sequence.1130 (:tag :sacla)
 (assert-true (eql (search '(a b c) '(a b c a b c) :start2 1) 3)))
(define-test sacla-must-sequence.1131 (:tag :sacla)
 (assert-true (eql (search '(a b c) '(a b c a b c) :start2 1 :end2 nil) 3)))
(define-test sacla-must-sequence.1132 (:tag :sacla)
 (assert-true
  (eql (search '(a b c) '(a b c a b c) :start1 1 :start2 1 :end2 nil) 1)))
(define-test sacla-must-sequence.1133 (:tag :sacla)
 (assert-true
  (eql (search '(a b c) '(a b c a b c) :start1 1 :end1 nil :start2 1 :end2 nil)
       1)))
(define-test sacla-must-sequence.1134 (:tag :sacla)
 (assert-true (null (search '(a b c) '(a b c a b c) :start2 0 :end2 0))))
(define-test sacla-must-sequence.1135 (:tag :sacla)
 (assert-true (null (search '(a b c) '(a b c a b c) :start2 1 :end2 1))))
(define-test sacla-must-sequence.1136 (:tag :sacla)
 (assert-true (null (search '(a b c) '(a b c a b c) :start2 2 :end2 2))))
(define-test sacla-must-sequence.1137 (:tag :sacla)
 (assert-true (null (search '(a b c) '(a b c a b c) :start2 3 :end2 3))))
(define-test sacla-must-sequence.1138 (:tag :sacla)
 (assert-true (null (search '(a b c) '(a b c a b c) :start2 4 :end2 4))))
(define-test sacla-must-sequence.1139 (:tag :sacla)
 (assert-true (null (search '(a b c) '(a b c a b c) :start2 5 :end2 5))))
(define-test sacla-must-sequence.1140 (:tag :sacla)
 (assert-true (null (search '(a b c) '(a b c a b c) :start2 6 :end2 6))))
(define-test sacla-must-sequence.1141 (:tag :sacla)
 (assert-true (eql (search '(a b c) '(a b c a b c)) 0)))
(define-test sacla-must-sequence.1142 (:tag :sacla)
 (assert-true (eql (search '(a b c) '(a b c a b c) :from-end t) 3)))
(define-test sacla-must-sequence.1143 (:tag :sacla)
 (assert-true (eql (search '(a b c) '(a b c a b c) :start2 3 :end2 6) 3)))
(define-test sacla-must-sequence.1144 (:tag :sacla)
 (assert-true
  (eql (search '(a b c) '(a b c a b c) :start2 3 :end2 6 :from-end t) 3)))
(define-test sacla-must-sequence.1145 (:tag :sacla)
 (assert-true
  (eql (search '(a b c) '(a b c a b c) :start1 0 :end1 2 :start2 0 :end2 6) 0)))
(define-test sacla-must-sequence.1146 (:tag :sacla)
 (assert-true
  (eql
   (search '(a b c)
           '(a b c a b c)
           :start1 0
           :end1 2
           :start2 0
           :end2 6
           :from-end t)
   3)))
(define-test sacla-must-sequence.1147 (:tag :sacla)
 (assert-true
  (eql (search '(a b c) '(a b c a b c) :start1 0 :end1 0 :start2 0 :end2 0) 0)))
(define-test sacla-must-sequence.1148 (:tag :sacla)
 (assert-true
  (eql (search '(a b c) '(a b c a b c) :start1 1 :end1 1 :start2 0 :end2 0) 0)))
(define-test sacla-must-sequence.1149 (:tag :sacla)
 (assert-true
  (eql (search '(a b c) '(a b c a b c) :start1 2 :end1 2 :start2 0 :end2 0) 0)))
(define-test sacla-must-sequence.1150 (:tag :sacla)
 (assert-true
  (eql (search '(a b c) '(a b c a b c) :start1 3 :end1 3 :start2 0 :end2 0) 0)))
(define-test sacla-must-sequence.1151 (:tag :sacla)
 (assert-true
  (eql (search '(a b c) '(a b c a b c) :start1 0 :end1 0 :start2 1 :end2 1) 1)))
(define-test sacla-must-sequence.1152 (:tag :sacla)
 (assert-true
  (eql (search '(a b c) '(a b c a b c) :start1 1 :end1 1 :start2 1 :end2 1) 1)))
(define-test sacla-must-sequence.1153 (:tag :sacla)
 (assert-true
  (eql (search '(a b c) '(a b c a b c) :start1 2 :end1 2 :start2 1 :end2 1) 1)))
(define-test sacla-must-sequence.1154 (:tag :sacla)
 (assert-true
  (eql (search '(a b c) '(a b c a b c) :start1 3 :end1 3 :start2 1 :end2 1) 1)))
(define-test sacla-must-sequence.1155 (:tag :sacla)
 (assert-true
  (eql (search '(a b c) '(a b c a b c) :start1 0 :end1 0 :start2 6 :end2 6) 6)))
(define-test sacla-must-sequence.1156 (:tag :sacla)
 (assert-true
  (eql (search '(a b c) '(a b c a b c) :start1 1 :end1 1 :start2 6 :end2 6) 6)))
(define-test sacla-must-sequence.1157 (:tag :sacla)
 (assert-true
  (eql (search '(a b c) '(a b c a b c) :start1 2 :end1 2 :start2 6 :end2 6) 6)))
(define-test sacla-must-sequence.1158 (:tag :sacla)
 (assert-true
  (eql (search '(a b c) '(a b c a b c) :start1 3 :end1 3 :start2 6 :end2 6) 6)))
(define-test sacla-must-sequence.1159 (:tag :sacla)
 (assert-true
  (eql
   (search '(a b c)
           '(a b c a b c)
           :start1 0
           :end1 0
           :start2 0
           :end2 0
           :from-end t)
   0)))
(define-test sacla-must-sequence.1160 (:tag :sacla)
 (assert-true
  (eql
   (search '(a b c)
           '(a b c a b c)
           :start1 1
           :end1 1
           :start2 0
           :end2 0
           :from-end t)
   0)))
(define-test sacla-must-sequence.1161 (:tag :sacla)
 (assert-true
  (eql
   (search '(a b c)
           '(a b c a b c)
           :start1 2
           :end1 2
           :start2 0
           :end2 0
           :from-end t)
   0)))
(define-test sacla-must-sequence.1162 (:tag :sacla)
 (assert-true
  (eql
   (search '(a b c)
           '(a b c a b c)
           :start1 3
           :end1 3
           :start2 0
           :end2 0
           :from-end t)
   0)))
(define-test sacla-must-sequence.1163 (:tag :sacla)
 (assert-true
  (eql
   (search '(a b c)
           '(a b c a b c)
           :start1 0
           :end1 0
           :start2 1
           :end2 1
           :from-end t)
   1)))
(define-test sacla-must-sequence.1164 (:tag :sacla)
 (assert-true
  (eql
   (search '(a b c)
           '(a b c a b c)
           :start1 1
           :end1 1
           :start2 1
           :end2 1
           :from-end t)
   1)))
(define-test sacla-must-sequence.1165 (:tag :sacla)
 (assert-true
  (eql
   (search '(a b c)
           '(a b c a b c)
           :start1 2
           :end1 2
           :start2 1
           :end2 1
           :from-end t)
   1)))
(define-test sacla-must-sequence.1166 (:tag :sacla)
 (assert-true
  (eql
   (search '(a b c)
           '(a b c a b c)
           :start1 3
           :end1 3
           :start2 1
           :end2 1
           :from-end t)
   1)))
(define-test sacla-must-sequence.1167 (:tag :sacla)
 (assert-true
  (eql
   (search '(a b c)
           '(a b c a b c)
           :start1 0
           :end1 0
           :start2 6
           :end2 6
           :from-end t)
   6)))
(define-test sacla-must-sequence.1168 (:tag :sacla)
 (assert-true
  (eql
   (search '(a b c)
           '(a b c a b c)
           :start1 1
           :end1 1
           :start2 6
           :end2 6
           :from-end t)
   6)))
(define-test sacla-must-sequence.1169 (:tag :sacla)
 (assert-true
  (eql
   (search '(a b c)
           '(a b c a b c)
           :start1 2
           :end1 2
           :start2 6
           :end2 6
           :from-end t)
   6)))
(define-test sacla-must-sequence.1170 (:tag :sacla)
 (assert-true
  (eql
   (search '(a b c)
           '(a b c a b c)
           :start1 3
           :end1 3
           :start2 6
           :end2 6
           :from-end t)
   6)))
(define-test sacla-must-sequence.1171 (:tag :sacla)
 (assert-true (null (search '(#\a #\b #\c) '(#\A #\B #\C)))))
(define-test sacla-must-sequence.1172 (:tag :sacla)
 (assert-true
  (eql (search '(#\a #\b #\c) '(#\A #\B #\C) :test #'char-equal) 0)))
(define-test sacla-must-sequence.1173 (:tag :sacla)
 (assert-true
  (eql
   (search '(#\a #\b #\c) '(#\A #\B #\C) :test-not (complement #'char-equal))
   0)))
(define-test sacla-must-sequence.1174 (:tag :sacla)
 (assert-true (eql (search '(#\a #\b) '(#\a #\b #\x #\y #\z)) 0)))
(define-test sacla-must-sequence.1175 (:tag :sacla)
 (assert-true (eql (search '(#\a #\b) '(#\a #\b #\x #\y #\z) :test #'char<) 1)))
(define-test sacla-must-sequence.1176 (:tag :sacla)
 (assert-true
  (eql
   (search '(#\a #\b) '(#\a #\b #\x #\y #\z) :test-not (complement #'char<))
   1)))
(define-test sacla-must-sequence.1177 (:tag :sacla)
 (assert-true
  (eql
   (search '(#\a #\b)
           '(#\a #\b #\x #\y #\z)
           :test-not (complement #'char<)
           :from-end t)
   3)))
(define-test sacla-must-sequence.1178 (:tag :sacla)
 (assert-true (null (search '((a) (b)) '((x) (y) (z) (a) (b) (c))))))
(define-test sacla-must-sequence.1179 (:tag :sacla)
 (assert-true
  (eql (search '((a) (b)) '((x) (y) (z) (a) (b) (c)) :key #'car) 3)))
(define-test sacla-must-sequence.1180 (:tag :sacla)
 (assert-true
  (eql (search '((a) (b)) '((a) (b) (c) (x) (y) (z) (a) (b) (c)) :key #'car)
       0)))
(define-test sacla-must-sequence.1181 (:tag :sacla)
 (assert-true
  (eql
   (search '((a) (b))
           '((a) (b) (c) (x) (y) (z) (a) (b) (c))
           :key #'car
           :from-end t)
   6)))
(define-test sacla-must-sequence.1182 (:tag :sacla)
 (assert-true
  (eql
   (search '((a a) (b b)) '((a) (b) (c) (x) (y) (z) (a) (b) (c)) :key #'car)
   0)))
(define-test sacla-must-sequence.1183 (:tag :sacla)
 (assert-true
  (eql
   (search '((a a) (b b))
           '((a nil) (b t) (c nil) (x) (y) (z) (a 0) (b 1) (c 2))
           :key #'car
           :from-end t)
   6)))
(define-test sacla-must-sequence.1184 (:tag :sacla)
 (assert-true
  (eql
   (search '(("a" a) ("b" b))
           '(("a" nil) ("b" t) ("c" nil) ("x") ("y") ("z") ("A" 0) ("B" 1)
             ("C" 2))
           :start1 1
           :end1 2
           :start2 3
           :end2 nil
           :key #'car
           :test #'string-equal
           :from-end t)
   7)))
(define-test sacla-must-sequence.1185 (:tag :sacla)
 (assert-true (eql (search #() 'nil) 0)))
(define-test sacla-must-sequence.1186 (:tag :sacla)
 (assert-true (null (search #(a b c) '(x y z)))))
(define-test sacla-must-sequence.1187 (:tag :sacla)
 (assert-true (eql (search #() '(x y z)) 0)))
(define-test sacla-must-sequence.1188 (:tag :sacla)
 (assert-true (eql (search #(a) '(a)) 0)))
(define-test sacla-must-sequence.1189 (:tag :sacla)
 (assert-true (eql (search #(a b c) '(a b c x y z)) 0)))
(define-test sacla-must-sequence.1190 (:tag :sacla)
 (assert-true (eql (search #(a b c) '(x a b c y z)) 1)))
(define-test sacla-must-sequence.1191 (:tag :sacla)
 (assert-true (eql (search #(a b c) '(x y a b c z)) 2)))
(define-test sacla-must-sequence.1192 (:tag :sacla)
 (assert-true (eql (search #(a b c) '(x y z a b c)) 3)))
(define-test sacla-must-sequence.1193 (:tag :sacla)
 (assert-true (eql (search #(a b c) '(a b c a b c) :start2 1) 3)))
(define-test sacla-must-sequence.1194 (:tag :sacla)
 (assert-true (eql (search #(a b c) '(a b c a b c) :start2 1 :end2 nil) 3)))
(define-test sacla-must-sequence.1195 (:tag :sacla)
 (assert-true
  (eql (search #(a b c) '(a b c a b c) :start1 1 :start2 1 :end2 nil) 1)))
(define-test sacla-must-sequence.1196 (:tag :sacla)
 (assert-true
  (eql (search #(a b c) '(a b c a b c) :start1 1 :end1 nil :start2 1 :end2 nil)
       1)))
(define-test sacla-must-sequence.1197 (:tag :sacla)
 (assert-true (null (search #(a b c) '(a b c a b c) :start2 0 :end2 0))))
(define-test sacla-must-sequence.1198 (:tag :sacla)
 (assert-true (null (search #(a b c) '(a b c a b c) :start2 1 :end2 1))))
(define-test sacla-must-sequence.1199 (:tag :sacla)
 (assert-true (null (search #(a b c) '(a b c a b c) :start2 2 :end2 2))))
(define-test sacla-must-sequence.1200 (:tag :sacla)
 (assert-true (null (search #(a b c) '(a b c a b c) :start2 3 :end2 3))))
(define-test sacla-must-sequence.1201 (:tag :sacla)
 (assert-true (null (search #(a b c) '(a b c a b c) :start2 4 :end2 4))))
(define-test sacla-must-sequence.1202 (:tag :sacla)
 (assert-true (null (search #(a b c) '(a b c a b c) :start2 5 :end2 5))))
(define-test sacla-must-sequence.1203 (:tag :sacla)
 (assert-true (null (search #(a b c) '(a b c a b c) :start2 6 :end2 6))))
(define-test sacla-must-sequence.1204 (:tag :sacla)
 (assert-true (eql (search #(a b c) '(a b c a b c)) 0)))
(define-test sacla-must-sequence.1205 (:tag :sacla)
 (assert-true (eql (search #(a b c) '(a b c a b c) :from-end t) 3)))
(define-test sacla-must-sequence.1206 (:tag :sacla)
 (assert-true (eql (search #(a b c) '(a b c a b c) :start2 3 :end2 6) 3)))
(define-test sacla-must-sequence.1207 (:tag :sacla)
 (assert-true
  (eql (search #(a b c) '(a b c a b c) :start2 3 :end2 6 :from-end t) 3)))
(define-test sacla-must-sequence.1208 (:tag :sacla)
 (assert-true
  (eql (search #(a b c) '(a b c a b c) :start1 0 :end1 2 :start2 0 :end2 6) 0)))
(define-test sacla-must-sequence.1209 (:tag :sacla)
 (assert-true
  (eql
   (search #(a b c)
           '(a b c a b c)
           :start1 0
           :end1 2
           :start2 0
           :end2 6
           :from-end t)
   3)))
(define-test sacla-must-sequence.1210 (:tag :sacla)
 (assert-true
  (eql (search #(a b c) '(a b c a b c) :start1 0 :end1 0 :start2 0 :end2 0) 0)))
(define-test sacla-must-sequence.1211 (:tag :sacla)
 (assert-true
  (eql (search #(a b c) '(a b c a b c) :start1 1 :end1 1 :start2 0 :end2 0) 0)))
(define-test sacla-must-sequence.1212 (:tag :sacla)
 (assert-true
  (eql (search #(a b c) '(a b c a b c) :start1 2 :end1 2 :start2 0 :end2 0) 0)))
(define-test sacla-must-sequence.1213 (:tag :sacla)
 (assert-true
  (eql (search #(a b c) '(a b c a b c) :start1 3 :end1 3 :start2 0 :end2 0) 0)))
(define-test sacla-must-sequence.1214 (:tag :sacla)
 (assert-true
  (eql (search #(a b c) '(a b c a b c) :start1 0 :end1 0 :start2 1 :end2 1) 1)))
(define-test sacla-must-sequence.1215 (:tag :sacla)
 (assert-true
  (eql (search #(a b c) '(a b c a b c) :start1 1 :end1 1 :start2 1 :end2 1) 1)))
(define-test sacla-must-sequence.1216 (:tag :sacla)
 (assert-true
  (eql (search #(a b c) '(a b c a b c) :start1 2 :end1 2 :start2 1 :end2 1) 1)))
(define-test sacla-must-sequence.1217 (:tag :sacla)
 (assert-true
  (eql (search #(a b c) '(a b c a b c) :start1 3 :end1 3 :start2 1 :end2 1) 1)))
(define-test sacla-must-sequence.1218 (:tag :sacla)
 (assert-true
  (eql (search #(a b c) '(a b c a b c) :start1 0 :end1 0 :start2 6 :end2 6) 6)))
(define-test sacla-must-sequence.1219 (:tag :sacla)
 (assert-true
  (eql (search #(a b c) '(a b c a b c) :start1 1 :end1 1 :start2 6 :end2 6) 6)))
(define-test sacla-must-sequence.1220 (:tag :sacla)
 (assert-true
  (eql (search #(a b c) '(a b c a b c) :start1 2 :end1 2 :start2 6 :end2 6) 6)))
(define-test sacla-must-sequence.1221 (:tag :sacla)
 (assert-true
  (eql (search #(a b c) '(a b c a b c) :start1 3 :end1 3 :start2 6 :end2 6) 6)))
(define-test sacla-must-sequence.1222 (:tag :sacla)
 (assert-true
  (eql
   (search #(a b c)
           '(a b c a b c)
           :start1 0
           :end1 0
           :start2 0
           :end2 0
           :from-end t)
   0)))
(define-test sacla-must-sequence.1223 (:tag :sacla)
 (assert-true
  (eql
   (search #(a b c)
           '(a b c a b c)
           :start1 1
           :end1 1
           :start2 0
           :end2 0
           :from-end t)
   0)))
(define-test sacla-must-sequence.1224 (:tag :sacla)
 (assert-true
  (eql
   (search #(a b c)
           '(a b c a b c)
           :start1 2
           :end1 2
           :start2 0
           :end2 0
           :from-end t)
   0)))
(define-test sacla-must-sequence.1225 (:tag :sacla)
 (assert-true
  (eql
   (search #(a b c)
           '(a b c a b c)
           :start1 3
           :end1 3
           :start2 0
           :end2 0
           :from-end t)
   0)))
(define-test sacla-must-sequence.1226 (:tag :sacla)
 (assert-true
  (eql
   (search #(a b c)
           '(a b c a b c)
           :start1 0
           :end1 0
           :start2 1
           :end2 1
           :from-end t)
   1)))
(define-test sacla-must-sequence.1227 (:tag :sacla)
 (assert-true
  (eql
   (search #(a b c)
           '(a b c a b c)
           :start1 1
           :end1 1
           :start2 1
           :end2 1
           :from-end t)
   1)))
(define-test sacla-must-sequence.1228 (:tag :sacla)
 (assert-true
  (eql
   (search #(a b c)
           '(a b c a b c)
           :start1 2
           :end1 2
           :start2 1
           :end2 1
           :from-end t)
   1)))
(define-test sacla-must-sequence.1229 (:tag :sacla)
 (assert-true
  (eql
   (search #(a b c)
           '(a b c a b c)
           :start1 3
           :end1 3
           :start2 1
           :end2 1
           :from-end t)
   1)))
(define-test sacla-must-sequence.1230 (:tag :sacla)
 (assert-true
  (eql
   (search #(a b c)
           '(a b c a b c)
           :start1 0
           :end1 0
           :start2 6
           :end2 6
           :from-end t)
   6)))
(define-test sacla-must-sequence.1231 (:tag :sacla)
 (assert-true
  (eql
   (search #(a b c)
           '(a b c a b c)
           :start1 1
           :end1 1
           :start2 6
           :end2 6
           :from-end t)
   6)))
(define-test sacla-must-sequence.1232 (:tag :sacla)
 (assert-true
  (eql
   (search #(a b c)
           '(a b c a b c)
           :start1 2
           :end1 2
           :start2 6
           :end2 6
           :from-end t)
   6)))
(define-test sacla-must-sequence.1233 (:tag :sacla)
 (assert-true
  (eql
   (search #(a b c)
           '(a b c a b c)
           :start1 3
           :end1 3
           :start2 6
           :end2 6
           :from-end t)
   6)))
(define-test sacla-must-sequence.1234 (:tag :sacla)
 (assert-true (null (search #(#\a #\b #\c) '(#\A #\B #\C)))))
(define-test sacla-must-sequence.1235 (:tag :sacla)
 (assert-true
  (eql (search #(#\a #\b #\c) '(#\A #\B #\C) :test #'char-equal) 0)))
(define-test sacla-must-sequence.1236 (:tag :sacla)
 (assert-true
  (eql
   (search #(#\a #\b #\c) '(#\A #\B #\C) :test-not (complement #'char-equal))
   0)))
(define-test sacla-must-sequence.1237 (:tag :sacla)
 (assert-true (eql (search #(#\a #\b) '(#\a #\b #\x #\y #\z)) 0)))
(define-test sacla-must-sequence.1238 (:tag :sacla)
 (assert-true (eql (search #(#\a #\b) '(#\a #\b #\x #\y #\z) :test #'char<) 1)))
(define-test sacla-must-sequence.1239 (:tag :sacla)
 (assert-true
  (eql
   (search #(#\a #\b) '(#\a #\b #\x #\y #\z) :test-not (complement #'char<))
   1)))
(define-test sacla-must-sequence.1240 (:tag :sacla)
 (assert-true
  (eql
   (search #(#\a #\b)
           '(#\a #\b #\x #\y #\z)
           :test-not (complement #'char<)
           :from-end t)
   3)))
(define-test sacla-must-sequence.1241 (:tag :sacla)
 (assert-true (null (search #((a) (b)) '((x) (y) (z) (a) (b) (c))))))
(define-test sacla-must-sequence.1242 (:tag :sacla)
 (assert-true
  (eql (search #((a) (b)) '((x) (y) (z) (a) (b) (c)) :key #'car) 3)))
(define-test sacla-must-sequence.1243 (:tag :sacla)
 (assert-true
  (eql (search #((a) (b)) '((a) (b) (c) (x) (y) (z) (a) (b) (c)) :key #'car)
       0)))
(define-test sacla-must-sequence.1244 (:tag :sacla)
 (assert-true
  (eql
   (search #((a) (b))
           '((a) (b) (c) (x) (y) (z) (a) (b) (c))
           :key #'car
           :from-end t)
   6)))
(define-test sacla-must-sequence.1245 (:tag :sacla)
 (assert-true
  (eql
   (search #((a a) (b b)) '((a) (b) (c) (x) (y) (z) (a) (b) (c)) :key #'car)
   0)))
(define-test sacla-must-sequence.1246 (:tag :sacla)
 (assert-true
  (eql
   (search #((a a) (b b))
           '((a nil) (b t) (c nil) (x) (y) (z) (a 0) (b 1) (c 2))
           :key #'car
           :from-end t)
   6)))
(define-test sacla-must-sequence.1247 (:tag :sacla)
 (assert-true
  (eql
   (search #(("a" a) ("b" b))
           '(("a" nil) ("b" t) ("c" nil) ("x") ("y") ("z") ("A" 0) ("B" 1)
             ("C" 2))
           :start1 1
           :end1 2
           :start2 3
           :end2 nil
           :key #'car
           :test #'string-equal
           :from-end t)
   7)))
(define-test sacla-must-sequence.1248 (:tag :sacla)
 (assert-true (eql (search 'nil #()) 0)))
(define-test sacla-must-sequence.1249 (:tag :sacla)
 (assert-true (null (search '(a b c) #(x y z)))))
(define-test sacla-must-sequence.1250 (:tag :sacla)
 (assert-true (eql (search 'nil #(x y z)) 0)))
(define-test sacla-must-sequence.1251 (:tag :sacla)
 (assert-true (eql (search '(a) #(a)) 0)))
(define-test sacla-must-sequence.1252 (:tag :sacla)
 (assert-true (eql (search '(a b c) #(a b c x y z)) 0)))
(define-test sacla-must-sequence.1253 (:tag :sacla)
 (assert-true (eql (search '(a b c) #(x a b c y z)) 1)))
(define-test sacla-must-sequence.1254 (:tag :sacla)
 (assert-true (eql (search '(a b c) #(x y a b c z)) 2)))
(define-test sacla-must-sequence.1255 (:tag :sacla)
 (assert-true (eql (search '(a b c) #(x y z a b c)) 3)))
(define-test sacla-must-sequence.1256 (:tag :sacla)
 (assert-true (eql (search '(a b c) #(a b c a b c) :start2 1) 3)))
(define-test sacla-must-sequence.1257 (:tag :sacla)
 (assert-true (eql (search '(a b c) #(a b c a b c) :start2 1 :end2 nil) 3)))
(define-test sacla-must-sequence.1258 (:tag :sacla)
 (assert-true
  (eql (search '(a b c) #(a b c a b c) :start1 1 :start2 1 :end2 nil) 1)))
(define-test sacla-must-sequence.1259 (:tag :sacla)
 (assert-true
  (eql (search '(a b c) #(a b c a b c) :start1 1 :end1 nil :start2 1 :end2 nil)
       1)))
(define-test sacla-must-sequence.1260 (:tag :sacla)
 (assert-true (null (search '(a b c) #(a b c a b c) :start2 0 :end2 0))))
(define-test sacla-must-sequence.1261 (:tag :sacla)
 (assert-true (null (search '(a b c) #(a b c a b c) :start2 1 :end2 1))))
(define-test sacla-must-sequence.1262 (:tag :sacla)
 (assert-true (null (search '(a b c) #(a b c a b c) :start2 2 :end2 2))))
(define-test sacla-must-sequence.1263 (:tag :sacla)
 (assert-true (null (search '(a b c) #(a b c a b c) :start2 3 :end2 3))))
(define-test sacla-must-sequence.1264 (:tag :sacla)
 (assert-true (null (search '(a b c) #(a b c a b c) :start2 4 :end2 4))))
(define-test sacla-must-sequence.1265 (:tag :sacla)
 (assert-true (null (search '(a b c) #(a b c a b c) :start2 5 :end2 5))))
(define-test sacla-must-sequence.1266 (:tag :sacla)
 (assert-true (null (search '(a b c) #(a b c a b c) :start2 6 :end2 6))))
(define-test sacla-must-sequence.1267 (:tag :sacla)
 (assert-true (eql (search '(a b c) #(a b c a b c)) 0)))
(define-test sacla-must-sequence.1268 (:tag :sacla)
 (assert-true (eql (search '(a b c) #(a b c a b c) :from-end t) 3)))
(define-test sacla-must-sequence.1269 (:tag :sacla)
 (assert-true (eql (search '(a b c) #(a b c a b c) :start2 3 :end2 6) 3)))
(define-test sacla-must-sequence.1270 (:tag :sacla)
 (assert-true
  (eql (search '(a b c) #(a b c a b c) :start2 3 :end2 6 :from-end t) 3)))
(define-test sacla-must-sequence.1271 (:tag :sacla)
 (assert-true
  (eql (search '(a b c) #(a b c a b c) :start1 0 :end1 2 :start2 0 :end2 6) 0)))
(define-test sacla-must-sequence.1272 (:tag :sacla)
 (assert-true
  (eql
   (search '(a b c)
           #(a b c a b c)
           :start1 0
           :end1 2
           :start2 0
           :end2 6
           :from-end t)
   3)))
(define-test sacla-must-sequence.1273 (:tag :sacla)
 (assert-true
  (eql (search '(a b c) #(a b c a b c) :start1 0 :end1 0 :start2 0 :end2 0) 0)))
(define-test sacla-must-sequence.1274 (:tag :sacla)
 (assert-true
  (eql (search '(a b c) #(a b c a b c) :start1 1 :end1 1 :start2 0 :end2 0) 0)))
(define-test sacla-must-sequence.1275 (:tag :sacla)
 (assert-true
  (eql (search '(a b c) #(a b c a b c) :start1 2 :end1 2 :start2 0 :end2 0) 0)))
(define-test sacla-must-sequence.1276 (:tag :sacla)
 (assert-true
  (eql (search '(a b c) #(a b c a b c) :start1 3 :end1 3 :start2 0 :end2 0) 0)))
(define-test sacla-must-sequence.1277 (:tag :sacla)
 (assert-true
  (eql (search '(a b c) #(a b c a b c) :start1 0 :end1 0 :start2 1 :end2 1) 1)))
(define-test sacla-must-sequence.1278 (:tag :sacla)
 (assert-true
  (eql (search '(a b c) #(a b c a b c) :start1 1 :end1 1 :start2 1 :end2 1) 1)))
(define-test sacla-must-sequence.1279 (:tag :sacla)
 (assert-true
  (eql (search '(a b c) #(a b c a b c) :start1 2 :end1 2 :start2 1 :end2 1) 1)))
(define-test sacla-must-sequence.1280 (:tag :sacla)
 (assert-true
  (eql (search '(a b c) #(a b c a b c) :start1 3 :end1 3 :start2 1 :end2 1) 1)))
(define-test sacla-must-sequence.1281 (:tag :sacla)
 (assert-true
  (eql (search '(a b c) #(a b c a b c) :start1 0 :end1 0 :start2 6 :end2 6) 6)))
(define-test sacla-must-sequence.1282 (:tag :sacla)
 (assert-true
  (eql (search '(a b c) #(a b c a b c) :start1 1 :end1 1 :start2 6 :end2 6) 6)))
(define-test sacla-must-sequence.1283 (:tag :sacla)
 (assert-true
  (eql (search '(a b c) #(a b c a b c) :start1 2 :end1 2 :start2 6 :end2 6) 6)))
(define-test sacla-must-sequence.1284 (:tag :sacla)
 (assert-true
  (eql (search '(a b c) #(a b c a b c) :start1 3 :end1 3 :start2 6 :end2 6) 6)))
(define-test sacla-must-sequence.1285 (:tag :sacla)
 (assert-true
  (eql
   (search '(a b c)
           #(a b c a b c)
           :start1 0
           :end1 0
           :start2 0
           :end2 0
           :from-end t)
   0)))
(define-test sacla-must-sequence.1286 (:tag :sacla)
 (assert-true
  (eql
   (search '(a b c)
           #(a b c a b c)
           :start1 1
           :end1 1
           :start2 0
           :end2 0
           :from-end t)
   0)))
(define-test sacla-must-sequence.1287 (:tag :sacla)
 (assert-true
  (eql
   (search '(a b c)
           #(a b c a b c)
           :start1 2
           :end1 2
           :start2 0
           :end2 0
           :from-end t)
   0)))
(define-test sacla-must-sequence.1288 (:tag :sacla)
 (assert-true
  (eql
   (search '(a b c)
           #(a b c a b c)
           :start1 3
           :end1 3
           :start2 0
           :end2 0
           :from-end t)
   0)))
(define-test sacla-must-sequence.1289 (:tag :sacla)
 (assert-true
  (eql
   (search '(a b c)
           #(a b c a b c)
           :start1 0
           :end1 0
           :start2 1
           :end2 1
           :from-end t)
   1)))
(define-test sacla-must-sequence.1290 (:tag :sacla)
 (assert-true
  (eql
   (search '(a b c)
           #(a b c a b c)
           :start1 1
           :end1 1
           :start2 1
           :end2 1
           :from-end t)
   1)))
(define-test sacla-must-sequence.1291 (:tag :sacla)
 (assert-true
  (eql
   (search '(a b c)
           #(a b c a b c)
           :start1 2
           :end1 2
           :start2 1
           :end2 1
           :from-end t)
   1)))
(define-test sacla-must-sequence.1292 (:tag :sacla)
 (assert-true
  (eql
   (search '(a b c)
           #(a b c a b c)
           :start1 3
           :end1 3
           :start2 1
           :end2 1
           :from-end t)
   1)))
(define-test sacla-must-sequence.1293 (:tag :sacla)
 (assert-true
  (eql
   (search '(a b c)
           #(a b c a b c)
           :start1 0
           :end1 0
           :start2 6
           :end2 6
           :from-end t)
   6)))
(define-test sacla-must-sequence.1294 (:tag :sacla)
 (assert-true
  (eql
   (search '(a b c)
           #(a b c a b c)
           :start1 1
           :end1 1
           :start2 6
           :end2 6
           :from-end t)
   6)))
(define-test sacla-must-sequence.1295 (:tag :sacla)
 (assert-true
  (eql
   (search '(a b c)
           #(a b c a b c)
           :start1 2
           :end1 2
           :start2 6
           :end2 6
           :from-end t)
   6)))
(define-test sacla-must-sequence.1296 (:tag :sacla)
 (assert-true
  (eql
   (search '(a b c)
           #(a b c a b c)
           :start1 3
           :end1 3
           :start2 6
           :end2 6
           :from-end t)
   6)))
(define-test sacla-must-sequence.1297 (:tag :sacla)
 (assert-true (null (search '(#\a #\b #\c) #(#\A #\B #\C)))))
(define-test sacla-must-sequence.1298 (:tag :sacla)
 (assert-true
  (eql (search '(#\a #\b #\c) #(#\A #\B #\C) :test #'char-equal) 0)))
(define-test sacla-must-sequence.1299 (:tag :sacla)
 (assert-true
  (eql
   (search '(#\a #\b #\c) #(#\A #\B #\C) :test-not (complement #'char-equal))
   0)))
(define-test sacla-must-sequence.1300 (:tag :sacla)
 (assert-true (eql (search '(#\a #\b) #(#\a #\b #\x #\y #\z)) 0)))
(define-test sacla-must-sequence.1301 (:tag :sacla)
 (assert-true (eql (search '(#\a #\b) #(#\a #\b #\x #\y #\z) :test #'char<) 1)))
(define-test sacla-must-sequence.1302 (:tag :sacla)
 (assert-true
  (eql
   (search '(#\a #\b) #(#\a #\b #\x #\y #\z) :test-not (complement #'char<))
   1)))
(define-test sacla-must-sequence.1303 (:tag :sacla)
 (assert-true
  (eql
   (search '(#\a #\b)
           #(#\a #\b #\x #\y #\z)
           :test-not (complement #'char<)
           :from-end t)
   3)))
(define-test sacla-must-sequence.1304 (:tag :sacla)
 (assert-true (null (search '((a) (b)) #((x) (y) (z) (a) (b) (c))))))
(define-test sacla-must-sequence.1305 (:tag :sacla)
 (assert-true
  (eql (search '((a) (b)) #((x) (y) (z) (a) (b) (c)) :key #'car) 3)))
(define-test sacla-must-sequence.1306 (:tag :sacla)
 (assert-true
  (eql (search '((a) (b)) #((a) (b) (c) (x) (y) (z) (a) (b) (c)) :key #'car)
       0)))
(define-test sacla-must-sequence.1307 (:tag :sacla)
 (assert-true
  (eql
   (search '((a) (b))
           #((a) (b) (c) (x) (y) (z) (a) (b) (c))
           :key #'car
           :from-end t)
   6)))
(define-test sacla-must-sequence.1308 (:tag :sacla)
 (assert-true
  (eql
   (search '((a a) (b b)) #((a) (b) (c) (x) (y) (z) (a) (b) (c)) :key #'car)
   0)))
(define-test sacla-must-sequence.1309 (:tag :sacla)
 (assert-true
  (eql
   (search '((a a) (b b))
           #((a nil) (b t) (c nil) (x) (y) (z) (a 0) (b 1) (c 2))
           :key #'car
           :from-end t)
   6)))
(define-test sacla-must-sequence.1310 (:tag :sacla)
 (assert-true
  (eql
   (search '(("a" a) ("b" b))
           #(("a" nil) ("b" t) ("c" nil) ("x") ("y") ("z") ("A" 0) ("B" 1)
             ("C" 2))
           :start1 1
           :end1 2
           :start2 3
           :end2 nil
           :key #'car
           :test #'string-equal
           :from-end t)
   7)))
(define-test sacla-must-sequence.1311 (:tag :sacla)
 (assert-true (eql (search #() #()) 0)))
(define-test sacla-must-sequence.1312 (:tag :sacla)
 (assert-true (null (search #(a b c) #(x y z)))))
(define-test sacla-must-sequence.1313 (:tag :sacla)
 (assert-true (eql (search #() #(x y z)) 0)))
(define-test sacla-must-sequence.1314 (:tag :sacla)
 (assert-true (eql (search #(a) #(a)) 0)))
(define-test sacla-must-sequence.1315 (:tag :sacla)
 (assert-true (eql (search #(a b c) #(a b c x y z)) 0)))
(define-test sacla-must-sequence.1316 (:tag :sacla)
 (assert-true (eql (search #(a b c) #(x a b c y z)) 1)))
(define-test sacla-must-sequence.1317 (:tag :sacla)
 (assert-true (eql (search #(a b c) #(x y a b c z)) 2)))
(define-test sacla-must-sequence.1318 (:tag :sacla)
 (assert-true (eql (search #(a b c) #(x y z a b c)) 3)))
(define-test sacla-must-sequence.1319 (:tag :sacla)
 (assert-true (eql (search #(a b c) #(a b c a b c) :start2 1) 3)))
(define-test sacla-must-sequence.1320 (:tag :sacla)
 (assert-true (eql (search #(a b c) #(a b c a b c) :start2 1 :end2 nil) 3)))
(define-test sacla-must-sequence.1321 (:tag :sacla)
 (assert-true
  (eql (search #(a b c) #(a b c a b c) :start1 1 :start2 1 :end2 nil) 1)))
(define-test sacla-must-sequence.1322 (:tag :sacla)
 (assert-true
  (eql (search #(a b c) #(a b c a b c) :start1 1 :end1 nil :start2 1 :end2 nil)
       1)))
(define-test sacla-must-sequence.1323 (:tag :sacla)
 (assert-true (null (search #(a b c) #(a b c a b c) :start2 0 :end2 0))))
(define-test sacla-must-sequence.1324 (:tag :sacla)
 (assert-true (null (search #(a b c) #(a b c a b c) :start2 1 :end2 1))))
(define-test sacla-must-sequence.1325 (:tag :sacla)
 (assert-true (null (search #(a b c) #(a b c a b c) :start2 2 :end2 2))))
(define-test sacla-must-sequence.1326 (:tag :sacla)
 (assert-true (null (search #(a b c) #(a b c a b c) :start2 3 :end2 3))))
(define-test sacla-must-sequence.1327 (:tag :sacla)
 (assert-true (null (search #(a b c) #(a b c a b c) :start2 4 :end2 4))))
(define-test sacla-must-sequence.1328 (:tag :sacla)
 (assert-true (null (search #(a b c) #(a b c a b c) :start2 5 :end2 5))))
(define-test sacla-must-sequence.1329 (:tag :sacla)
 (assert-true (null (search #(a b c) #(a b c a b c) :start2 6 :end2 6))))
(define-test sacla-must-sequence.1330 (:tag :sacla)
 (assert-true (eql (search #(a b c) #(a b c a b c)) 0)))
(define-test sacla-must-sequence.1331 (:tag :sacla)
 (assert-true (eql (search #(a b c) #(a b c a b c) :from-end t) 3)))
(define-test sacla-must-sequence.1332 (:tag :sacla)
 (assert-true (eql (search #(a b c) #(a b c a b c) :start2 3 :end2 6) 3)))
(define-test sacla-must-sequence.1333 (:tag :sacla)
 (assert-true
  (eql (search #(a b c) #(a b c a b c) :start2 3 :end2 6 :from-end t) 3)))
(define-test sacla-must-sequence.1334 (:tag :sacla)
 (assert-true
  (eql (search #(a b c) #(a b c a b c) :start1 0 :end1 2 :start2 0 :end2 6) 0)))
(define-test sacla-must-sequence.1335 (:tag :sacla)
 (assert-true
  (eql
   (search #(a b c)
           #(a b c a b c)
           :start1 0
           :end1 2
           :start2 0
           :end2 6
           :from-end t)
   3)))
(define-test sacla-must-sequence.1336 (:tag :sacla)
 (assert-true
  (eql (search #(a b c) #(a b c a b c) :start1 0 :end1 0 :start2 0 :end2 0) 0)))
(define-test sacla-must-sequence.1337 (:tag :sacla)
 (assert-true
  (eql (search #(a b c) #(a b c a b c) :start1 1 :end1 1 :start2 0 :end2 0) 0)))
(define-test sacla-must-sequence.1338 (:tag :sacla)
 (assert-true
  (eql (search #(a b c) #(a b c a b c) :start1 2 :end1 2 :start2 0 :end2 0) 0)))
(define-test sacla-must-sequence.1339 (:tag :sacla)
 (assert-true
  (eql (search #(a b c) #(a b c a b c) :start1 3 :end1 3 :start2 0 :end2 0) 0)))
(define-test sacla-must-sequence.1340 (:tag :sacla)
 (assert-true
  (eql (search #(a b c) #(a b c a b c) :start1 0 :end1 0 :start2 1 :end2 1) 1)))
(define-test sacla-must-sequence.1341 (:tag :sacla)
 (assert-true
  (eql (search #(a b c) #(a b c a b c) :start1 1 :end1 1 :start2 1 :end2 1) 1)))
(define-test sacla-must-sequence.1342 (:tag :sacla)
 (assert-true
  (eql (search #(a b c) #(a b c a b c) :start1 2 :end1 2 :start2 1 :end2 1) 1)))
(define-test sacla-must-sequence.1343 (:tag :sacla)
 (assert-true
  (eql (search #(a b c) #(a b c a b c) :start1 3 :end1 3 :start2 1 :end2 1) 1)))
(define-test sacla-must-sequence.1344 (:tag :sacla)
 (assert-true
  (eql (search #(a b c) #(a b c a b c) :start1 0 :end1 0 :start2 6 :end2 6) 6)))
(define-test sacla-must-sequence.1345 (:tag :sacla)
 (assert-true
  (eql (search #(a b c) #(a b c a b c) :start1 1 :end1 1 :start2 6 :end2 6) 6)))
(define-test sacla-must-sequence.1346 (:tag :sacla)
 (assert-true
  (eql (search #(a b c) #(a b c a b c) :start1 2 :end1 2 :start2 6 :end2 6) 6)))
(define-test sacla-must-sequence.1347 (:tag :sacla)
 (assert-true
  (eql (search #(a b c) #(a b c a b c) :start1 3 :end1 3 :start2 6 :end2 6) 6)))
(define-test sacla-must-sequence.1348 (:tag :sacla)
 (assert-true
  (eql
   (search #(a b c)
           #(a b c a b c)
           :start1 0
           :end1 0
           :start2 0
           :end2 0
           :from-end t)
   0)))
(define-test sacla-must-sequence.1349 (:tag :sacla)
 (assert-true
  (eql
   (search #(a b c)
           #(a b c a b c)
           :start1 1
           :end1 1
           :start2 0
           :end2 0
           :from-end t)
   0)))
(define-test sacla-must-sequence.1350 (:tag :sacla)
 (assert-true
  (eql
   (search #(a b c)
           #(a b c a b c)
           :start1 2
           :end1 2
           :start2 0
           :end2 0
           :from-end t)
   0)))
(define-test sacla-must-sequence.1351 (:tag :sacla)
 (assert-true
  (eql
   (search #(a b c)
           #(a b c a b c)
           :start1 3
           :end1 3
           :start2 0
           :end2 0
           :from-end t)
   0)))
(define-test sacla-must-sequence.1352 (:tag :sacla)
 (assert-true
  (eql
   (search #(a b c)
           #(a b c a b c)
           :start1 0
           :end1 0
           :start2 1
           :end2 1
           :from-end t)
   1)))
(define-test sacla-must-sequence.1353 (:tag :sacla)
 (assert-true
  (eql
   (search #(a b c)
           #(a b c a b c)
           :start1 1
           :end1 1
           :start2 1
           :end2 1
           :from-end t)
   1)))
(define-test sacla-must-sequence.1354 (:tag :sacla)
 (assert-true
  (eql
   (search #(a b c)
           #(a b c a b c)
           :start1 2
           :end1 2
           :start2 1
           :end2 1
           :from-end t)
   1)))
(define-test sacla-must-sequence.1355 (:tag :sacla)
 (assert-true
  (eql
   (search #(a b c)
           #(a b c a b c)
           :start1 3
           :end1 3
           :start2 1
           :end2 1
           :from-end t)
   1)))
(define-test sacla-must-sequence.1356 (:tag :sacla)
 (assert-true
  (eql
   (search #(a b c)
           #(a b c a b c)
           :start1 0
           :end1 0
           :start2 6
           :end2 6
           :from-end t)
   6)))
(define-test sacla-must-sequence.1357 (:tag :sacla)
 (assert-true
  (eql
   (search #(a b c)
           #(a b c a b c)
           :start1 1
           :end1 1
           :start2 6
           :end2 6
           :from-end t)
   6)))
(define-test sacla-must-sequence.1358 (:tag :sacla)
 (assert-true
  (eql
   (search #(a b c)
           #(a b c a b c)
           :start1 2
           :end1 2
           :start2 6
           :end2 6
           :from-end t)
   6)))
(define-test sacla-must-sequence.1359 (:tag :sacla)
 (assert-true
  (eql
   (search #(a b c)
           #(a b c a b c)
           :start1 3
           :end1 3
           :start2 6
           :end2 6
           :from-end t)
   6)))
(define-test sacla-must-sequence.1360 (:tag :sacla)
 (assert-true (null (search #(#\a #\b #\c) #(#\A #\B #\C)))))
(define-test sacla-must-sequence.1361 (:tag :sacla)
 (assert-true
  (eql (search #(#\a #\b #\c) #(#\A #\B #\C) :test #'char-equal) 0)))
(define-test sacla-must-sequence.1362 (:tag :sacla)
 (assert-true
  (eql
   (search #(#\a #\b #\c) #(#\A #\B #\C) :test-not (complement #'char-equal))
   0)))
(define-test sacla-must-sequence.1363 (:tag :sacla)
 (assert-true (eql (search #(#\a #\b) #(#\a #\b #\x #\y #\z)) 0)))
(define-test sacla-must-sequence.1364 (:tag :sacla)
 (assert-true (eql (search #(#\a #\b) #(#\a #\b #\x #\y #\z) :test #'char<) 1)))
(define-test sacla-must-sequence.1365 (:tag :sacla)
 (assert-true
  (eql
   (search #(#\a #\b) #(#\a #\b #\x #\y #\z) :test-not (complement #'char<))
   1)))
(define-test sacla-must-sequence.1366 (:tag :sacla)
 (assert-true
  (eql
   (search #(#\a #\b)
           #(#\a #\b #\x #\y #\z)
           :test-not (complement #'char<)
           :from-end t)
   3)))
(define-test sacla-must-sequence.1367 (:tag :sacla)
 (assert-true (null (search #((a) (b)) #((x) (y) (z) (a) (b) (c))))))
(define-test sacla-must-sequence.1368 (:tag :sacla)
 (assert-true
  (eql (search #((a) (b)) #((x) (y) (z) (a) (b) (c)) :key #'car) 3)))
(define-test sacla-must-sequence.1369 (:tag :sacla)
 (assert-true
  (eql (search #((a) (b)) #((a) (b) (c) (x) (y) (z) (a) (b) (c)) :key #'car)
       0)))
(define-test sacla-must-sequence.1370 (:tag :sacla)
 (assert-true
  (eql
   (search #((a) (b))
           #((a) (b) (c) (x) (y) (z) (a) (b) (c))
           :key #'car
           :from-end t)
   6)))
(define-test sacla-must-sequence.1371 (:tag :sacla)
 (assert-true
  (eql
   (search #((a a) (b b)) #((a) (b) (c) (x) (y) (z) (a) (b) (c)) :key #'car)
   0)))
(define-test sacla-must-sequence.1372 (:tag :sacla)
 (assert-true
  (eql
   (search #((a a) (b b))
           #((a nil) (b t) (c nil) (x) (y) (z) (a 0) (b 1) (c 2))
           :key #'car
           :from-end t)
   6)))
(define-test sacla-must-sequence.1373 (:tag :sacla)
 (assert-true
  (eql
   (search #(("a" a) ("b" b))
           #(("a" nil) ("b" t) ("c" nil) ("x") ("y") ("z") ("A" 0) ("B" 1)
             ("C" 2))
           :start1 1
           :end1 2
           :start2 3
           :end2 nil
           :key #'car
           :test #'string-equal
           :from-end t)
   7)))
(define-test sacla-must-sequence.1374 (:tag :sacla)
 (assert-true (null (search "peace" "LOVE&PEACE"))))
(define-test sacla-must-sequence.1375 (:tag :sacla)
 (assert-true (eql (search "peace" "LOVE&PEACE" :test #'char-equal) 5)))
(define-test sacla-must-sequence.1376 (:tag :sacla)
 (assert-true
  (eql
   (search (concatenate 'simple-vector "peace")
           (concatenate 'list "LOVE&PEACE")
           :test #'char-equal)
   5)))
(define-test sacla-must-sequence.1377 (:tag :sacla)
 (assert-true
  (eql
   (search (concatenate 'list "peace")
           (concatenate 'vector "LOVE&PEACE")
           :test #'char-equal)
   5)))
(define-test sacla-must-sequence.1378 (:tag :sacla)
 (assert-true
  (eql
   (search (concatenate 'vector "peace")
           (concatenate 'vector "LOVE&PEACE")
           :test #'char-equal)
   5)))
(define-test sacla-must-sequence.1379 (:tag :sacla)
 (assert-true (eql (search #*10 #*010101) 1)))
(define-test sacla-must-sequence.1380 (:tag :sacla)
 (assert-true (eql (search #*10 #*010101 :from-end t) 3)))
(define-test sacla-must-sequence.1381 (:tag :sacla)
 (assert-true (null (search "PeAcE" "LoVe&pEaCe"))))
(define-test sacla-must-sequence.1382 (:tag :sacla)
 (assert-true (eql (search "PeAcE" "LoVe&pEaCe" :key #'char-upcase) 5)))
(define-test sacla-must-sequence.1383 (:tag :sacla)
 (assert-true (eql (search "abc" "abc xyz abc" :from-end t) 8)))
(define-test sacla-must-sequence.1384 (:tag :sacla)
 (assert-true
  (eql (search "abc" "abc xyz abc xyz abc xyz abc" :start2 8 :end2 19) 8)))
(define-test sacla-must-sequence.1385 (:tag :sacla)
 (assert-true
  (eql
   (search "abc" "abc xyz abc xyz abc xyz abc" :from-end t :start2 8 :end2 19)
   16)))
(define-test sacla-must-sequence.1386 (:tag :sacla)
 (assert-true (eql (mismatch "abcd" "ABCDE" :test #'char-equal) 4)))
(define-test sacla-must-sequence.1387 (:tag :sacla)
 (assert-true (eql (mismatch '(3 2 1 1 2 3) '(1 2 3) :from-end t) 3)))
(define-test sacla-must-sequence.1388 (:tag :sacla)
 (assert-true (null (mismatch '(1 2 3) '(2 3 4) :test-not #'eq :key #'oddp))))
(define-test sacla-must-sequence.1389 (:tag :sacla)
 (assert-true (null (mismatch '(1 2 3 4 5 6) '(3 4 5 6 7) :start1 2 :end2 4))))
(define-test sacla-must-sequence.1390 (:tag :sacla)
 (assert-true (null (mismatch 'nil 'nil))))
(define-test sacla-must-sequence.1391 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c) '(x y z)) 0)))
(define-test sacla-must-sequence.1392 (:tag :sacla)
 (assert-true (eql (mismatch 'nil '(x y z)) 0)))
(define-test sacla-must-sequence.1393 (:tag :sacla)
 (assert-true (eql (mismatch '(x y z) 'nil) 0)))
(define-test sacla-must-sequence.1394 (:tag :sacla)
 (assert-true (null (mismatch '(a) '(a)))))
(define-test sacla-must-sequence.1395 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c x y z) '(a b c)) 3)))
(define-test sacla-must-sequence.1396 (:tag :sacla)
 (assert-true (null (mismatch '(a b c) '(a b c)))))
(define-test sacla-must-sequence.1397 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c d e f) '(a b c)) 3)))
(define-test sacla-must-sequence.1398 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c) '(a b c d e f)) 3)))
(define-test sacla-must-sequence.1399 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c) '(a b x)) 2)))
(define-test sacla-must-sequence.1400 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c) '(a x c)) 1)))
(define-test sacla-must-sequence.1401 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c) '(x b c)) 0)))
(define-test sacla-must-sequence.1402 (:tag :sacla)
 (assert-true (eql (mismatch '(x y z a b c x y z) '(a b c) :start1 3) 6)))
(define-test sacla-must-sequence.1403 (:tag :sacla)
 (assert-true
  (eql (mismatch '(x y z a b c x y z) '(a b c) :start1 3 :end1 nil) 6)))
(define-test sacla-must-sequence.1404 (:tag :sacla)
 (assert-true
  (eql (mismatch '(x y z a b c x y z) '(a b c) :start1 3 :end1 4) 4)))
(define-test sacla-must-sequence.1405 (:tag :sacla)
 (assert-true
  (eql (mismatch '(x y z a b c x y z) '(a b c) :start1 3 :end1 3) 3)))
(define-test sacla-must-sequence.1406 (:tag :sacla)
 (assert-true (null (mismatch '(x y z) 'nil :start1 0 :end1 0))))
(define-test sacla-must-sequence.1407 (:tag :sacla)
 (assert-true (null (mismatch '(x y z) 'nil :start1 1 :end1 1))))
(define-test sacla-must-sequence.1408 (:tag :sacla)
 (assert-true (null (mismatch '(x y z) 'nil :start1 2 :end1 2))))
(define-test sacla-must-sequence.1409 (:tag :sacla)
 (assert-true (null (mismatch '(x y z) 'nil :start1 3 :end1 3))))
(define-test sacla-must-sequence.1410 (:tag :sacla)
 (assert-true
  (null (mismatch '(x y z) 'nil :start1 0 :end1 0 :start2 0 :end2 0))))
(define-test sacla-must-sequence.1411 (:tag :sacla)
 (assert-true
  (null (mismatch '(x y z) 'nil :start1 1 :end1 1 :start2 1 :end2 1))))
(define-test sacla-must-sequence.1412 (:tag :sacla)
 (assert-true
  (null (mismatch '(x y z) 'nil :start1 2 :end1 2 :start2 2 :end2 2))))
(define-test sacla-must-sequence.1413 (:tag :sacla)
 (assert-true
  (null (mismatch '(x y z) 'nil :start1 3 :end1 3 :start2 3 :end2 3))))
(define-test sacla-must-sequence.1414 (:tag :sacla)
 (assert-true
  (null (mismatch '(x y z) 'nil :start1 0 :end1 0 :start2 3 :end2 3))))
(define-test sacla-must-sequence.1415 (:tag :sacla)
 (assert-true
  (null (mismatch '(x y z) 'nil :start1 1 :end1 1 :start2 2 :end2 2))))
(define-test sacla-must-sequence.1416 (:tag :sacla)
 (assert-true
  (null (mismatch '(x y z) 'nil :start1 2 :end1 2 :start2 1 :end2 1))))
(define-test sacla-must-sequence.1417 (:tag :sacla)
 (assert-true
  (null (mismatch '(x y z) 'nil :start1 3 :end1 3 :start2 0 :end2 0))))
(define-test sacla-must-sequence.1418 (:tag :sacla)
 (assert-true (eql (mismatch '(x y z) '(a b c) :start1 0 :end1 0) 0)))
(define-test sacla-must-sequence.1419 (:tag :sacla)
 (assert-true (eql (mismatch '(x y z) '(a b c) :start1 1 :end1 1) 1)))
(define-test sacla-must-sequence.1420 (:tag :sacla)
 (assert-true (eql (mismatch '(x y z) '(a b c) :start1 2 :end1 2) 2)))
(define-test sacla-must-sequence.1421 (:tag :sacla)
 (assert-true (eql (mismatch '(x y z) '(a b c) :start1 3 :end1 3) 3)))
(define-test sacla-must-sequence.1422 (:tag :sacla)
 (assert-true (eql (mismatch '(x y z) '(x y z) :start1 0 :end1 1) 1)))
(define-test sacla-must-sequence.1423 (:tag :sacla)
 (assert-true (eql (mismatch '(x y z) '(x y z) :start1 0 :end1 2) 2)))
(define-test sacla-must-sequence.1424 (:tag :sacla)
 (assert-true (eql (mismatch '(x y z) '(x y z z) :start1 0 :end1 3) 3)))
(define-test sacla-must-sequence.1425 (:tag :sacla)
 (assert-true (null (mismatch '(x y z) '(x y z) :start1 0 :end1 3))))
(define-test sacla-must-sequence.1426 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c x y z) '(x y z a b c)) 0)))
(define-test sacla-must-sequence.1427 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c x y z) '(x y z a b c) :start1 3) 6)))
(define-test sacla-must-sequence.1428 (:tag :sacla)
 (assert-true
  (eql (mismatch '(a b c x y z a b c) '(x y z a b c x y z) :start1 3) 9)))
(define-test sacla-must-sequence.1429 (:tag :sacla)
 (assert-true
  (eql (mismatch '(a b c x y z a b c) '(x y z a b c x y z) :start1 6) 6)))
(define-test sacla-must-sequence.1430 (:tag :sacla)
 (assert-true
  (eql (mismatch '(a b c x y z a b c) '(x y z a b c x y z) :start1 6 :start2 3)
       9)))
(define-test sacla-must-sequence.1431 (:tag :sacla)
 (assert-true
  (eql (mismatch '(a b c x y z a b c) '(x y z a b c x y z) :start1 0 :start2 3)
       6)))
(define-test sacla-must-sequence.1432 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c) '(a b c x y z)) 3)))
(define-test sacla-must-sequence.1433 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c) '(x a b c y z)) 0)))
(define-test sacla-must-sequence.1434 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c) '(x a b c y z) :start2 1) 3)))
(define-test sacla-must-sequence.1435 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c) '(x a b c y z) :start2 1 :end2 nil) 3)))
(define-test sacla-must-sequence.1436 (:tag :sacla)
 (assert-true (null (mismatch '(a b c) '(x a b c y z) :start2 1 :end2 4))))
(define-test sacla-must-sequence.1437 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c d e) '(c d)) 0)))
(define-test sacla-must-sequence.1438 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c d e) '(c d) :start1 2) 4)))
(define-test sacla-must-sequence.1439 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c d e) '(c d) :start1 2 :end1 3) 3)))
(define-test sacla-must-sequence.1440 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c d e) '(c d) :start1 2 :start2 1) 2)))
(define-test sacla-must-sequence.1441 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c d e) '(c d) :start1 3 :start2 1) 4)))
(define-test sacla-must-sequence.1442 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c d e) '(c d) :start1 2 :end2 1) 3)))
(define-test sacla-must-sequence.1443 (:tag :sacla)
 (assert-true
  (null (mismatch '(a b c d) '(a b c d) :start1 1 :end1 2 :start2 1 :end2 2))))
(define-test sacla-must-sequence.1444 (:tag :sacla)
 (assert-true
  (null (mismatch '(a b c d) '(a b c d) :start1 1 :end1 3 :start2 1 :end2 3))))
(define-test sacla-must-sequence.1445 (:tag :sacla)
 (assert-true
  (null (mismatch '(a b c d) '(a b c d) :start1 1 :end1 4 :start2 1 :end2 4))))
(define-test sacla-must-sequence.1446 (:tag :sacla)
 (assert-true
  (eql (mismatch '(a b c d) '(a b c d) :start1 1 :end1 nil :start2 1 :end2 1)
       1)))
(define-test sacla-must-sequence.1447 (:tag :sacla)
 (assert-true
  (eql (mismatch '(a b c d) '(a b c d) :start1 1 :end1 nil :start2 1 :end2 2)
       2)))
(define-test sacla-must-sequence.1448 (:tag :sacla)
 (assert-true
  (eql (mismatch '(a b c d) '(a b c d) :start1 1 :end1 nil :start2 1 :end2 3)
       3)))
(define-test sacla-must-sequence.1449 (:tag :sacla)
 (assert-true
  (null
   (mismatch '(a b c d) '(a b c d) :start1 1 :end1 nil :start2 1 :end2 4))))
(define-test sacla-must-sequence.1450 (:tag :sacla)
 (assert-true
  (eql (mismatch '(a b c d) '(a b c d) :start1 1 :end1 1 :start2 1) 1)))
(define-test sacla-must-sequence.1451 (:tag :sacla)
 (assert-true
  (eql (mismatch '(a b c d) '(a b c d) :start1 1 :end1 2 :start2 1) 2)))
(define-test sacla-must-sequence.1452 (:tag :sacla)
 (assert-true
  (eql (mismatch '(a b c d) '(a b c d) :start1 1 :end1 3 :start2 1) 3)))
(define-test sacla-must-sequence.1453 (:tag :sacla)
 (assert-true
  (null (mismatch '(a b c d) '(a b c d) :start1 1 :end1 4 :start2 1))))
(define-test sacla-must-sequence.1454 (:tag :sacla)
 (assert-true (null (mismatch '(a b c) '(a b c) :from-end t))))
(define-test sacla-must-sequence.1455 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c d) '(a b c) :from-end t) 4)))
(define-test sacla-must-sequence.1456 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c) '(c) :from-end t) 2)))
(define-test sacla-must-sequence.1457 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c) '(z a b c) :from-end t) 0)))
(define-test sacla-must-sequence.1458 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c) '(x y z a b c) :from-end t) 0)))
(define-test sacla-must-sequence.1459 (:tag :sacla)
 (assert-true (eql (mismatch '(x y z a b c) '(a b c) :from-end t) 3)))
(define-test sacla-must-sequence.1460 (:tag :sacla)
 (assert-true (eql (mismatch '(x y z a b c) '(a b c) :end1 3 :from-end t) 3)))
(define-test sacla-must-sequence.1461 (:tag :sacla)
 (assert-true (eql (mismatch '(x y z a b c) '(a b c) :end1 5 :from-end t) 5)))
(define-test sacla-must-sequence.1462 (:tag :sacla)
 (assert-true
  (eql (mismatch '(x y z a b c x y z) '(a b c) :end1 6 :from-end t) 3)))
(define-test sacla-must-sequence.1463 (:tag :sacla)
 (assert-true
  (eql (mismatch '(x y z a b c x y z) '(a b c) :start1 2 :end1 6 :from-end t)
       3)))
(define-test sacla-must-sequence.1464 (:tag :sacla)
 (assert-true
  (eql
   (mismatch '(x y z a b c x y z)
             '(a b c)
             :from-end t
             :start1 2
             :end1 5
             :start2 1
             :end2 2)
   4)))
(define-test sacla-must-sequence.1465 (:tag :sacla)
 (assert-true
  (eql
   (mismatch '(x y z a b c x y z) '(a b c) :start1 2 :end1 5 :start2 1 :end2 2)
   2)))
(define-test sacla-must-sequence.1466 (:tag :sacla)
 (assert-true (eql (mismatch '((a) (b) (c)) '((a) (b) (c))) 0)))
(define-test sacla-must-sequence.1467 (:tag :sacla)
 (assert-true (null (mismatch '((a) (b) (c)) '((a) (b) (c)) :key #'car))))
(define-test sacla-must-sequence.1468 (:tag :sacla)
 (assert-true (null (mismatch '((a) (b) (c)) '((a) (b) (c)) :test #'equal))))
(define-test sacla-must-sequence.1469 (:tag :sacla)
 (assert-true (eql (mismatch '(#(a) #(b) #(c)) '(#(a) #(b) #(c))) 0)))
(define-test sacla-must-sequence.1470 (:tag :sacla)
 (assert-true
  (null (mismatch '(#(a) #(b) #(c)) '(#(a) #(b) #(c)) :test #'equalp))))
(define-test sacla-must-sequence.1471 (:tag :sacla)
 (assert-true (eql (mismatch '((a) (b) (c) (d)) '((a) (b) (c)) :key #'car) 3)))
(define-test sacla-must-sequence.1472 (:tag :sacla)
 (assert-true (eql (mismatch '((a) (b) (c)) '((a) (b) (c) (d)) :key #'car) 3)))
(define-test sacla-must-sequence.1473 (:tag :sacla)
 (assert-true (eql (mismatch '(#\a #\b #\c) '(#\A #\B #\C)) 0)))
(define-test sacla-must-sequence.1474 (:tag :sacla)
 (assert-true
  (null (mismatch '(#\a #\b #\c) '(#\A #\B #\C) :key #'char-upcase))))
(define-test sacla-must-sequence.1475 (:tag :sacla)
 (assert-true
  (null (mismatch '(#\a #\b #\c) '(#\A #\B #\C) :key #'char-downcase))))
(define-test sacla-must-sequence.1476 (:tag :sacla)
 (assert-true
  (null
   (mismatch '(#\a #\b #\c)
             '(#\A #\B #\C)
             :key #'char-upcase
             :start1 1
             :end1 2
             :start2 1
             :end2 2))))
(define-test sacla-must-sequence.1477 (:tag :sacla)
 (assert-true
  (null
   (mismatch '(#\a #\b #\c)
             '(#\A #\B #\C)
             :key #'char-upcase
             :start1 2
             :start2 2))))
(define-test sacla-must-sequence.1478 (:tag :sacla)
 (assert-true
  (eql (mismatch '((a b c) (b c d) (d e f)) '((b b c) (c c d) (e e f))) 0)))
(define-test sacla-must-sequence.1479 (:tag :sacla)
 (assert-true
  (eql
   (mismatch '((a b c) (b c d) (d e f)) '((b b c) (c c d) (e e f)) :key #'cdr)
   0)))
(define-test sacla-must-sequence.1480 (:tag :sacla)
 (assert-true
  (null
   (mismatch '((a b c) (b c d) (d e f))
             '((b b c) (c c d) (e e f))
             :key #'cdr
             :test #'equal))))
(define-test sacla-must-sequence.1481 (:tag :sacla)
 (assert-true
  (eql
   (mismatch '((a b c) (b c d) (d e f) (e f g))
             '((b b c) (c c d) (e e f))
             :key #'cdr
             :test #'equal)
   3)))
(define-test sacla-must-sequence.1482 (:tag :sacla)
 (assert-true
  (eql
   (mismatch '((a b c) (b c d) (d e f) (e f g))
             '((b b c) (c c d) (e e f))
             :key #'cdr
             :test #'equal
             :from-end t)
   4)))
(define-test sacla-must-sequence.1483 (:tag :sacla)
 (assert-true
  (eql
   (mismatch '((a a a) (a b c) (b c d) (d e f))
             '((b b c) (c c d) (e e f))
             :key #'cdr
             :test #'equal
             :from-end t)
   1)))
(define-test sacla-must-sequence.1484 (:tag :sacla)
 (assert-true
  (null
   (mismatch '((a a a) (a b c) (b c d) (d e f) (e f g))
             '((b b c) (c c d) (e e f))
             :key #'cdr
             :test #'equal
             :from-end t
             :start1 1
             :end1 4))))
(define-test sacla-must-sequence.1485 (:tag :sacla)
 (assert-true
  (eql
   (mismatch '((a a a) (a b c) (b c d) (d e f) (e f g))
             '((b b c) (c c d) (e e f))
             :key #'cdr
             :test #'equal
             :from-end t
             :start1 1)
   5)))
(define-test sacla-must-sequence.1486 (:tag :sacla)
 (assert-true
  (eql
   (mismatch '((a a a) (a b c) (b c d) (d e f) (e f g))
             '((b b c) (c c d) (e e f))
             :key #'cdr
             :test #'equal
             :from-end t
             :end1 3
             :start2 1
             :end2 2)
   2)))
(define-test sacla-must-sequence.1487 (:tag :sacla)
 (assert-true (null (mismatch #() 'nil))))
(define-test sacla-must-sequence.1488 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c) '(x y z)) 0)))
(define-test sacla-must-sequence.1489 (:tag :sacla)
 (assert-true (eql (mismatch #() '(x y z)) 0)))
(define-test sacla-must-sequence.1490 (:tag :sacla)
 (assert-true (eql (mismatch #(x y z) 'nil) 0)))
(define-test sacla-must-sequence.1491 (:tag :sacla)
 (assert-true (null (mismatch #(a) '(a)))))
(define-test sacla-must-sequence.1492 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c x y z) '(a b c)) 3)))
(define-test sacla-must-sequence.1493 (:tag :sacla)
 (assert-true (null (mismatch #(a b c) '(a b c)))))
(define-test sacla-must-sequence.1494 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c d e f) '(a b c)) 3)))
(define-test sacla-must-sequence.1495 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c) '(a b c d e f)) 3)))
(define-test sacla-must-sequence.1496 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c) '(a b x)) 2)))
(define-test sacla-must-sequence.1497 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c) '(a x c)) 1)))
(define-test sacla-must-sequence.1498 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c) '(x b c)) 0)))
(define-test sacla-must-sequence.1499 (:tag :sacla)
 (assert-true (eql (mismatch #(x y z a b c x y z) '(a b c) :start1 3) 6)))
(define-test sacla-must-sequence.1500 (:tag :sacla)
 (assert-true
  (eql (mismatch #(x y z a b c x y z) '(a b c) :start1 3 :end1 nil) 6)))
(define-test sacla-must-sequence.1501 (:tag :sacla)
 (assert-true
  (eql (mismatch #(x y z a b c x y z) '(a b c) :start1 3 :end1 4) 4)))
(define-test sacla-must-sequence.1502 (:tag :sacla)
 (assert-true
  (eql (mismatch #(x y z a b c x y z) '(a b c) :start1 3 :end1 3) 3)))
(define-test sacla-must-sequence.1503 (:tag :sacla)
 (assert-true (null (mismatch #(x y z) 'nil :start1 0 :end1 0))))
(define-test sacla-must-sequence.1504 (:tag :sacla)
 (assert-true (null (mismatch #(x y z) 'nil :start1 1 :end1 1))))
(define-test sacla-must-sequence.1505 (:tag :sacla)
 (assert-true (null (mismatch #(x y z) 'nil :start1 2 :end1 2))))
(define-test sacla-must-sequence.1506 (:tag :sacla)
 (assert-true (null (mismatch #(x y z) 'nil :start1 3 :end1 3))))
(define-test sacla-must-sequence.1507 (:tag :sacla)
 (assert-true
  (null (mismatch #(x y z) 'nil :start1 0 :end1 0 :start2 0 :end2 0))))
(define-test sacla-must-sequence.1508 (:tag :sacla)
 (assert-true
  (null (mismatch #(x y z) 'nil :start1 1 :end1 1 :start2 1 :end2 1))))
(define-test sacla-must-sequence.1509 (:tag :sacla)
 (assert-true
  (null (mismatch #(x y z) 'nil :start1 2 :end1 2 :start2 2 :end2 2))))
(define-test sacla-must-sequence.1510 (:tag :sacla)
 (assert-true
  (null (mismatch #(x y z) 'nil :start1 3 :end1 3 :start2 3 :end2 3))))
(define-test sacla-must-sequence.1511 (:tag :sacla)
 (assert-true
  (null (mismatch #(x y z) 'nil :start1 0 :end1 0 :start2 3 :end2 3))))
(define-test sacla-must-sequence.1512 (:tag :sacla)
 (assert-true
  (null (mismatch #(x y z) 'nil :start1 1 :end1 1 :start2 2 :end2 2))))
(define-test sacla-must-sequence.1513 (:tag :sacla)
 (assert-true
  (null (mismatch #(x y z) 'nil :start1 2 :end1 2 :start2 1 :end2 1))))
(define-test sacla-must-sequence.1514 (:tag :sacla)
 (assert-true
  (null (mismatch #(x y z) 'nil :start1 3 :end1 3 :start2 0 :end2 0))))
(define-test sacla-must-sequence.1515 (:tag :sacla)
 (assert-true (eql (mismatch #(x y z) '(a b c) :start1 0 :end1 0) 0)))
(define-test sacla-must-sequence.1516 (:tag :sacla)
 (assert-true (eql (mismatch #(x y z) '(a b c) :start1 1 :end1 1) 1)))
(define-test sacla-must-sequence.1517 (:tag :sacla)
 (assert-true (eql (mismatch #(x y z) '(a b c) :start1 2 :end1 2) 2)))
(define-test sacla-must-sequence.1518 (:tag :sacla)
 (assert-true (eql (mismatch #(x y z) '(a b c) :start1 3 :end1 3) 3)))
(define-test sacla-must-sequence.1519 (:tag :sacla)
 (assert-true (eql (mismatch #(x y z) '(x y z) :start1 0 :end1 1) 1)))
(define-test sacla-must-sequence.1520 (:tag :sacla)
 (assert-true (eql (mismatch #(x y z) '(x y z) :start1 0 :end1 2) 2)))
(define-test sacla-must-sequence.1521 (:tag :sacla)
 (assert-true (eql (mismatch #(x y z) '(x y z z) :start1 0 :end1 3) 3)))
(define-test sacla-must-sequence.1522 (:tag :sacla)
 (assert-true (null (mismatch #(x y z) '(x y z) :start1 0 :end1 3))))
(define-test sacla-must-sequence.1523 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c x y z) '(x y z a b c)) 0)))
(define-test sacla-must-sequence.1524 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c x y z) '(x y z a b c) :start1 3) 6)))
(define-test sacla-must-sequence.1525 (:tag :sacla)
 (assert-true
  (eql (mismatch #(a b c x y z a b c) '(x y z a b c x y z) :start1 3) 9)))
(define-test sacla-must-sequence.1526 (:tag :sacla)
 (assert-true
  (eql (mismatch #(a b c x y z a b c) '(x y z a b c x y z) :start1 6) 6)))
(define-test sacla-must-sequence.1527 (:tag :sacla)
 (assert-true
  (eql (mismatch #(a b c x y z a b c) '(x y z a b c x y z) :start1 6 :start2 3)
       9)))
(define-test sacla-must-sequence.1528 (:tag :sacla)
 (assert-true
  (eql (mismatch #(a b c x y z a b c) '(x y z a b c x y z) :start1 0 :start2 3)
       6)))
(define-test sacla-must-sequence.1529 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c) '(a b c x y z)) 3)))
(define-test sacla-must-sequence.1530 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c) '(x a b c y z)) 0)))
(define-test sacla-must-sequence.1531 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c) '(x a b c y z) :start2 1) 3)))
(define-test sacla-must-sequence.1532 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c) '(x a b c y z) :start2 1 :end2 nil) 3)))
(define-test sacla-must-sequence.1533 (:tag :sacla)
 (assert-true (null (mismatch #(a b c) '(x a b c y z) :start2 1 :end2 4))))
(define-test sacla-must-sequence.1534 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c d e) '(c d)) 0)))
(define-test sacla-must-sequence.1535 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c d e) '(c d) :start1 2) 4)))
(define-test sacla-must-sequence.1536 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c d e) '(c d) :start1 2 :end1 3) 3)))
(define-test sacla-must-sequence.1537 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c d e) '(c d) :start1 2 :start2 1) 2)))
(define-test sacla-must-sequence.1538 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c d e) '(c d) :start1 3 :start2 1) 4)))
(define-test sacla-must-sequence.1539 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c d e) '(c d) :start1 2 :end2 1) 3)))
(define-test sacla-must-sequence.1540 (:tag :sacla)
 (assert-true
  (null (mismatch #(a b c d) '(a b c d) :start1 1 :end1 2 :start2 1 :end2 2))))
(define-test sacla-must-sequence.1541 (:tag :sacla)
 (assert-true
  (null (mismatch #(a b c d) '(a b c d) :start1 1 :end1 3 :start2 1 :end2 3))))
(define-test sacla-must-sequence.1542 (:tag :sacla)
 (assert-true
  (null (mismatch #(a b c d) '(a b c d) :start1 1 :end1 4 :start2 1 :end2 4))))
(define-test sacla-must-sequence.1543 (:tag :sacla)
 (assert-true
  (eql (mismatch #(a b c d) '(a b c d) :start1 1 :end1 nil :start2 1 :end2 1)
       1)))
(define-test sacla-must-sequence.1544 (:tag :sacla)
 (assert-true
  (eql (mismatch #(a b c d) '(a b c d) :start1 1 :end1 nil :start2 1 :end2 2)
       2)))
(define-test sacla-must-sequence.1545 (:tag :sacla)
 (assert-true
  (eql (mismatch #(a b c d) '(a b c d) :start1 1 :end1 nil :start2 1 :end2 3)
       3)))
(define-test sacla-must-sequence.1546 (:tag :sacla)
 (assert-true
  (null
   (mismatch #(a b c d) '(a b c d) :start1 1 :end1 nil :start2 1 :end2 4))))
(define-test sacla-must-sequence.1547 (:tag :sacla)
 (assert-true
  (eql (mismatch #(a b c d) '(a b c d) :start1 1 :end1 1 :start2 1) 1)))
(define-test sacla-must-sequence.1548 (:tag :sacla)
 (assert-true
  (eql (mismatch #(a b c d) '(a b c d) :start1 1 :end1 2 :start2 1) 2)))
(define-test sacla-must-sequence.1549 (:tag :sacla)
 (assert-true
  (eql (mismatch #(a b c d) '(a b c d) :start1 1 :end1 3 :start2 1) 3)))
(define-test sacla-must-sequence.1550 (:tag :sacla)
 (assert-true
  (null (mismatch #(a b c d) '(a b c d) :start1 1 :end1 4 :start2 1))))
(define-test sacla-must-sequence.1551 (:tag :sacla)
 (assert-true (null (mismatch #(a b c) '(a b c) :from-end t))))
(define-test sacla-must-sequence.1552 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c d) '(a b c) :from-end t) 4)))
(define-test sacla-must-sequence.1553 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c) '(c) :from-end t) 2)))
(define-test sacla-must-sequence.1554 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c) '(z a b c) :from-end t) 0)))
(define-test sacla-must-sequence.1555 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c) '(x y z a b c) :from-end t) 0)))
(define-test sacla-must-sequence.1556 (:tag :sacla)
 (assert-true (eql (mismatch #(x y z a b c) '(a b c) :from-end t) 3)))
(define-test sacla-must-sequence.1557 (:tag :sacla)
 (assert-true (eql (mismatch #(x y z a b c) '(a b c) :end1 3 :from-end t) 3)))
(define-test sacla-must-sequence.1558 (:tag :sacla)
 (assert-true (eql (mismatch #(x y z a b c) '(a b c) :end1 5 :from-end t) 5)))
(define-test sacla-must-sequence.1559 (:tag :sacla)
 (assert-true
  (eql (mismatch #(x y z a b c x y z) '(a b c) :end1 6 :from-end t) 3)))
(define-test sacla-must-sequence.1560 (:tag :sacla)
 (assert-true
  (eql (mismatch #(x y z a b c x y z) '(a b c) :start1 2 :end1 6 :from-end t)
       3)))
(define-test sacla-must-sequence.1561 (:tag :sacla)
 (assert-true
  (eql
   (mismatch #(x y z a b c x y z)
             '(a b c)
             :from-end t
             :start1 2
             :end1 5
             :start2 1
             :end2 2)
   4)))
(define-test sacla-must-sequence.1562 (:tag :sacla)
 (assert-true
  (eql
   (mismatch #(x y z a b c x y z) '(a b c) :start1 2 :end1 5 :start2 1 :end2 2)
   2)))
(define-test sacla-must-sequence.1563 (:tag :sacla)
 (assert-true (eql (mismatch #((a) (b) (c)) '((a) (b) (c))) 0)))
(define-test sacla-must-sequence.1564 (:tag :sacla)
 (assert-true (null (mismatch #((a) (b) (c)) '((a) (b) (c)) :key #'car))))
(define-test sacla-must-sequence.1565 (:tag :sacla)
 (assert-true (null (mismatch #((a) (b) (c)) '((a) (b) (c)) :test #'equal))))
(define-test sacla-must-sequence.1566 (:tag :sacla)
 (assert-true (eql (mismatch #(#(a) #(b) #(c)) '(#(a) #(b) #(c))) 0)))
(define-test sacla-must-sequence.1567 (:tag :sacla)
 (assert-true
  (null (mismatch #(#(a) #(b) #(c)) '(#(a) #(b) #(c)) :test #'equalp))))
(define-test sacla-must-sequence.1568 (:tag :sacla)
 (assert-true (eql (mismatch #((a) (b) (c) (d)) '((a) (b) (c)) :key #'car) 3)))
(define-test sacla-must-sequence.1569 (:tag :sacla)
 (assert-true (eql (mismatch #((a) (b) (c)) '((a) (b) (c) (d)) :key #'car) 3)))
(define-test sacla-must-sequence.1570 (:tag :sacla)
 (assert-true (eql (mismatch #(#\a #\b #\c) '(#\A #\B #\C)) 0)))
(define-test sacla-must-sequence.1571 (:tag :sacla)
 (assert-true
  (null (mismatch #(#\a #\b #\c) '(#\A #\B #\C) :key #'char-upcase))))
(define-test sacla-must-sequence.1572 (:tag :sacla)
 (assert-true
  (null (mismatch #(#\a #\b #\c) '(#\A #\B #\C) :key #'char-downcase))))
(define-test sacla-must-sequence.1573 (:tag :sacla)
 (assert-true
  (null
   (mismatch #(#\a #\b #\c)
             '(#\A #\B #\C)
             :key #'char-upcase
             :start1 1
             :end1 2
             :start2 1
             :end2 2))))
(define-test sacla-must-sequence.1574 (:tag :sacla)
 (assert-true
  (null
   (mismatch #(#\a #\b #\c)
             '(#\A #\B #\C)
             :key #'char-upcase
             :start1 2
             :start2 2))))
(define-test sacla-must-sequence.1575 (:tag :sacla)
 (assert-true
  (eql (mismatch #((a b c) (b c d) (d e f)) '((b b c) (c c d) (e e f))) 0)))
(define-test sacla-must-sequence.1576 (:tag :sacla)
 (assert-true
  (eql
   (mismatch #((a b c) (b c d) (d e f)) '((b b c) (c c d) (e e f)) :key #'cdr)
   0)))
(define-test sacla-must-sequence.1577 (:tag :sacla)
 (assert-true
  (null
   (mismatch #((a b c) (b c d) (d e f))
             '((b b c) (c c d) (e e f))
             :key #'cdr
             :test #'equal))))
(define-test sacla-must-sequence.1578 (:tag :sacla)
 (assert-true
  (eql
   (mismatch #((a b c) (b c d) (d e f) (e f g))
             '((b b c) (c c d) (e e f))
             :key #'cdr
             :test #'equal)
   3)))
(define-test sacla-must-sequence.1579 (:tag :sacla)
 (assert-true
  (eql
   (mismatch #((a b c) (b c d) (d e f) (e f g))
             '((b b c) (c c d) (e e f))
             :key #'cdr
             :test #'equal
             :from-end t)
   4)))
(define-test sacla-must-sequence.1580 (:tag :sacla)
 (assert-true
  (eql
   (mismatch #((a a a) (a b c) (b c d) (d e f))
             '((b b c) (c c d) (e e f))
             :key #'cdr
             :test #'equal
             :from-end t)
   1)))
(define-test sacla-must-sequence.1581 (:tag :sacla)
 (assert-true
  (null
   (mismatch #((a a a) (a b c) (b c d) (d e f) (e f g))
             '((b b c) (c c d) (e e f))
             :key #'cdr
             :test #'equal
             :from-end t
             :start1 1
             :end1 4))))
(define-test sacla-must-sequence.1582 (:tag :sacla)
 (assert-true
  (eql
   (mismatch #((a a a) (a b c) (b c d) (d e f) (e f g))
             '((b b c) (c c d) (e e f))
             :key #'cdr
             :test #'equal
             :from-end t
             :start1 1)
   5)))
(define-test sacla-must-sequence.1583 (:tag :sacla)
 (assert-true
  (eql
   (mismatch #((a a a) (a b c) (b c d) (d e f) (e f g))
             '((b b c) (c c d) (e e f))
             :key #'cdr
             :test #'equal
             :from-end t
             :end1 3
             :start2 1
             :end2 2)
   2)))
(define-test sacla-must-sequence.1584 (:tag :sacla)
 (assert-true (null (mismatch 'nil #()))))
(define-test sacla-must-sequence.1585 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c) #(x y z)) 0)))
(define-test sacla-must-sequence.1586 (:tag :sacla)
 (assert-true (eql (mismatch 'nil #(x y z)) 0)))
(define-test sacla-must-sequence.1587 (:tag :sacla)
 (assert-true (eql (mismatch '(x y z) #()) 0)))
(define-test sacla-must-sequence.1588 (:tag :sacla)
 (assert-true (null (mismatch '(a) #(a)))))
(define-test sacla-must-sequence.1589 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c x y z) #(a b c)) 3)))
(define-test sacla-must-sequence.1590 (:tag :sacla)
 (assert-true (null (mismatch '(a b c) #(a b c)))))
(define-test sacla-must-sequence.1591 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c d e f) #(a b c)) 3)))
(define-test sacla-must-sequence.1592 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c) #(a b c d e f)) 3)))
(define-test sacla-must-sequence.1593 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c) #(a b x)) 2)))
(define-test sacla-must-sequence.1594 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c) #(a x c)) 1)))
(define-test sacla-must-sequence.1595 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c) #(x b c)) 0)))
(define-test sacla-must-sequence.1596 (:tag :sacla)
 (assert-true (eql (mismatch '(x y z a b c x y z) #(a b c) :start1 3) 6)))
(define-test sacla-must-sequence.1597 (:tag :sacla)
 (assert-true
  (eql (mismatch '(x y z a b c x y z) #(a b c) :start1 3 :end1 nil) 6)))
(define-test sacla-must-sequence.1598 (:tag :sacla)
 (assert-true
  (eql (mismatch '(x y z a b c x y z) #(a b c) :start1 3 :end1 4) 4)))
(define-test sacla-must-sequence.1599 (:tag :sacla)
 (assert-true
  (eql (mismatch '(x y z a b c x y z) #(a b c) :start1 3 :end1 3) 3)))
(define-test sacla-must-sequence.1600 (:tag :sacla)
 (assert-true (null (mismatch '(x y z) #() :start1 0 :end1 0))))
(define-test sacla-must-sequence.1601 (:tag :sacla)
 (assert-true (null (mismatch '(x y z) #() :start1 1 :end1 1))))
(define-test sacla-must-sequence.1602 (:tag :sacla)
 (assert-true (null (mismatch '(x y z) #() :start1 2 :end1 2))))
(define-test sacla-must-sequence.1603 (:tag :sacla)
 (assert-true (null (mismatch '(x y z) #() :start1 3 :end1 3))))
(define-test sacla-must-sequence.1604 (:tag :sacla)
 (assert-true
  (null (mismatch '(x y z) #() :start1 0 :end1 0 :start2 0 :end2 0))))
(define-test sacla-must-sequence.1605 (:tag :sacla)
 (assert-true
  (null (mismatch '(x y z) #() :start1 1 :end1 1 :start2 1 :end2 1))))
(define-test sacla-must-sequence.1606 (:tag :sacla)
 (assert-true
  (null (mismatch '(x y z) #() :start1 2 :end1 2 :start2 2 :end2 2))))
(define-test sacla-must-sequence.1607 (:tag :sacla)
 (assert-true
  (null (mismatch '(x y z) #() :start1 3 :end1 3 :start2 3 :end2 3))))
(define-test sacla-must-sequence.1608 (:tag :sacla)
 (assert-true
  (null (mismatch '(x y z) #() :start1 0 :end1 0 :start2 3 :end2 3))))
(define-test sacla-must-sequence.1609 (:tag :sacla)
 (assert-true
  (null (mismatch '(x y z) #() :start1 1 :end1 1 :start2 2 :end2 2))))
(define-test sacla-must-sequence.1610 (:tag :sacla)
 (assert-true
  (null (mismatch '(x y z) #() :start1 2 :end1 2 :start2 1 :end2 1))))
(define-test sacla-must-sequence.1611 (:tag :sacla)
 (assert-true
  (null (mismatch '(x y z) #() :start1 3 :end1 3 :start2 0 :end2 0))))
(define-test sacla-must-sequence.1612 (:tag :sacla)
 (assert-true (eql (mismatch '(x y z) #(a b c) :start1 0 :end1 0) 0)))
(define-test sacla-must-sequence.1613 (:tag :sacla)
 (assert-true (eql (mismatch '(x y z) #(a b c) :start1 1 :end1 1) 1)))
(define-test sacla-must-sequence.1614 (:tag :sacla)
 (assert-true (eql (mismatch '(x y z) #(a b c) :start1 2 :end1 2) 2)))
(define-test sacla-must-sequence.1615 (:tag :sacla)
 (assert-true (eql (mismatch '(x y z) #(a b c) :start1 3 :end1 3) 3)))
(define-test sacla-must-sequence.1616 (:tag :sacla)
 (assert-true (eql (mismatch '(x y z) #(x y z) :start1 0 :end1 1) 1)))
(define-test sacla-must-sequence.1617 (:tag :sacla)
 (assert-true (eql (mismatch '(x y z) #(x y z) :start1 0 :end1 2) 2)))
(define-test sacla-must-sequence.1618 (:tag :sacla)
 (assert-true (eql (mismatch '(x y z) #(x y z z) :start1 0 :end1 3) 3)))
(define-test sacla-must-sequence.1619 (:tag :sacla)
 (assert-true (null (mismatch '(x y z) #(x y z) :start1 0 :end1 3))))
(define-test sacla-must-sequence.1620 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c x y z) #(x y z a b c)) 0)))
(define-test sacla-must-sequence.1621 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c x y z) #(x y z a b c) :start1 3) 6)))
(define-test sacla-must-sequence.1622 (:tag :sacla)
 (assert-true
  (eql (mismatch '(a b c x y z a b c) #(x y z a b c x y z) :start1 3) 9)))
(define-test sacla-must-sequence.1623 (:tag :sacla)
 (assert-true
  (eql (mismatch '(a b c x y z a b c) #(x y z a b c x y z) :start1 6) 6)))
(define-test sacla-must-sequence.1624 (:tag :sacla)
 (assert-true
  (eql (mismatch '(a b c x y z a b c) #(x y z a b c x y z) :start1 6 :start2 3)
       9)))
(define-test sacla-must-sequence.1625 (:tag :sacla)
 (assert-true
  (eql (mismatch '(a b c x y z a b c) #(x y z a b c x y z) :start1 0 :start2 3)
       6)))
(define-test sacla-must-sequence.1626 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c) #(a b c x y z)) 3)))
(define-test sacla-must-sequence.1627 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c) #(x a b c y z)) 0)))
(define-test sacla-must-sequence.1628 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c) #(x a b c y z) :start2 1) 3)))
(define-test sacla-must-sequence.1629 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c) #(x a b c y z) :start2 1 :end2 nil) 3)))
(define-test sacla-must-sequence.1630 (:tag :sacla)
 (assert-true (null (mismatch '(a b c) #(x a b c y z) :start2 1 :end2 4))))
(define-test sacla-must-sequence.1631 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c d e) #(c d)) 0)))
(define-test sacla-must-sequence.1632 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c d e) #(c d) :start1 2) 4)))
(define-test sacla-must-sequence.1633 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c d e) #(c d) :start1 2 :end1 3) 3)))
(define-test sacla-must-sequence.1634 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c d e) #(c d) :start1 2 :start2 1) 2)))
(define-test sacla-must-sequence.1635 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c d e) #(c d) :start1 3 :start2 1) 4)))
(define-test sacla-must-sequence.1636 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c d e) #(c d) :start1 2 :end2 1) 3)))
(define-test sacla-must-sequence.1637 (:tag :sacla)
 (assert-true
  (null (mismatch '(a b c d) #(a b c d) :start1 1 :end1 2 :start2 1 :end2 2))))
(define-test sacla-must-sequence.1638 (:tag :sacla)
 (assert-true
  (null (mismatch '(a b c d) #(a b c d) :start1 1 :end1 3 :start2 1 :end2 3))))
(define-test sacla-must-sequence.1639 (:tag :sacla)
 (assert-true
  (null (mismatch '(a b c d) #(a b c d) :start1 1 :end1 4 :start2 1 :end2 4))))
(define-test sacla-must-sequence.1640 (:tag :sacla)
 (assert-true
  (eql (mismatch '(a b c d) #(a b c d) :start1 1 :end1 nil :start2 1 :end2 1)
       1)))
(define-test sacla-must-sequence.1641 (:tag :sacla)
 (assert-true
  (eql (mismatch '(a b c d) #(a b c d) :start1 1 :end1 nil :start2 1 :end2 2)
       2)))
(define-test sacla-must-sequence.1642 (:tag :sacla)
 (assert-true
  (eql (mismatch '(a b c d) #(a b c d) :start1 1 :end1 nil :start2 1 :end2 3)
       3)))
(define-test sacla-must-sequence.1643 (:tag :sacla)
 (assert-true
  (null
   (mismatch '(a b c d) #(a b c d) :start1 1 :end1 nil :start2 1 :end2 4))))
(define-test sacla-must-sequence.1644 (:tag :sacla)
 (assert-true
  (eql (mismatch '(a b c d) #(a b c d) :start1 1 :end1 1 :start2 1) 1)))
(define-test sacla-must-sequence.1645 (:tag :sacla)
 (assert-true
  (eql (mismatch '(a b c d) #(a b c d) :start1 1 :end1 2 :start2 1) 2)))
(define-test sacla-must-sequence.1646 (:tag :sacla)
 (assert-true
  (eql (mismatch '(a b c d) #(a b c d) :start1 1 :end1 3 :start2 1) 3)))
(define-test sacla-must-sequence.1647 (:tag :sacla)
 (assert-true
  (null (mismatch '(a b c d) #(a b c d) :start1 1 :end1 4 :start2 1))))
(define-test sacla-must-sequence.1648 (:tag :sacla)
 (assert-true (null (mismatch '(a b c) #(a b c) :from-end t))))
(define-test sacla-must-sequence.1649 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c d) #(a b c) :from-end t) 4)))
(define-test sacla-must-sequence.1650 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c) #(c) :from-end t) 2)))
(define-test sacla-must-sequence.1651 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c) #(z a b c) :from-end t) 0)))
(define-test sacla-must-sequence.1652 (:tag :sacla)
 (assert-true (eql (mismatch '(a b c) #(x y z a b c) :from-end t) 0)))
(define-test sacla-must-sequence.1653 (:tag :sacla)
 (assert-true (eql (mismatch '(x y z a b c) #(a b c) :from-end t) 3)))
(define-test sacla-must-sequence.1654 (:tag :sacla)
 (assert-true (eql (mismatch '(x y z a b c) #(a b c) :end1 3 :from-end t) 3)))
(define-test sacla-must-sequence.1655 (:tag :sacla)
 (assert-true (eql (mismatch '(x y z a b c) #(a b c) :end1 5 :from-end t) 5)))
(define-test sacla-must-sequence.1656 (:tag :sacla)
 (assert-true
  (eql (mismatch '(x y z a b c x y z) #(a b c) :end1 6 :from-end t) 3)))
(define-test sacla-must-sequence.1657 (:tag :sacla)
 (assert-true
  (eql (mismatch '(x y z a b c x y z) #(a b c) :start1 2 :end1 6 :from-end t)
       3)))
(define-test sacla-must-sequence.1658 (:tag :sacla)
 (assert-true
  (eql
   (mismatch '(x y z a b c x y z)
             #(a b c)
             :from-end t
             :start1 2
             :end1 5
             :start2 1
             :end2 2)
   4)))
(define-test sacla-must-sequence.1659 (:tag :sacla)
 (assert-true
  (eql
   (mismatch '(x y z a b c x y z) #(a b c) :start1 2 :end1 5 :start2 1 :end2 2)
   2)))
(define-test sacla-must-sequence.1660 (:tag :sacla)
 (assert-true (eql (mismatch '((a) (b) (c)) #((a) (b) (c))) 0)))
(define-test sacla-must-sequence.1661 (:tag :sacla)
 (assert-true (null (mismatch '((a) (b) (c)) #((a) (b) (c)) :key #'car))))
(define-test sacla-must-sequence.1662 (:tag :sacla)
 (assert-true (null (mismatch '((a) (b) (c)) #((a) (b) (c)) :test #'equal))))
(define-test sacla-must-sequence.1663 (:tag :sacla)
 (assert-true (eql (mismatch '(#(a) #(b) #(c)) #(#(a) #(b) #(c))) 0)))
(define-test sacla-must-sequence.1664 (:tag :sacla)
 (assert-true
  (null (mismatch '(#(a) #(b) #(c)) #(#(a) #(b) #(c)) :test #'equalp))))
(define-test sacla-must-sequence.1665 (:tag :sacla)
 (assert-true (eql (mismatch '((a) (b) (c) (d)) #((a) (b) (c)) :key #'car) 3)))
(define-test sacla-must-sequence.1666 (:tag :sacla)
 (assert-true (eql (mismatch '((a) (b) (c)) #((a) (b) (c) (d)) :key #'car) 3)))
(define-test sacla-must-sequence.1667 (:tag :sacla)
 (assert-true (eql (mismatch '(#\a #\b #\c) #(#\A #\B #\C)) 0)))
(define-test sacla-must-sequence.1668 (:tag :sacla)
 (assert-true
  (null (mismatch '(#\a #\b #\c) #(#\A #\B #\C) :key #'char-upcase))))
(define-test sacla-must-sequence.1669 (:tag :sacla)
 (assert-true
  (null (mismatch '(#\a #\b #\c) #(#\A #\B #\C) :key #'char-downcase))))
(define-test sacla-must-sequence.1670 (:tag :sacla)
 (assert-true
  (null
   (mismatch '(#\a #\b #\c)
             #(#\A #\B #\C)
             :key #'char-upcase
             :start1 1
             :end1 2
             :start2 1
             :end2 2))))
(define-test sacla-must-sequence.1671 (:tag :sacla)
 (assert-true
  (null
   (mismatch '(#\a #\b #\c)
             #(#\A #\B #\C)
             :key #'char-upcase
             :start1 2
             :start2 2))))
(define-test sacla-must-sequence.1672 (:tag :sacla)
 (assert-true
  (eql (mismatch '((a b c) (b c d) (d e f)) #((b b c) (c c d) (e e f))) 0)))
(define-test sacla-must-sequence.1673 (:tag :sacla)
 (assert-true
  (eql
   (mismatch '((a b c) (b c d) (d e f)) #((b b c) (c c d) (e e f)) :key #'cdr)
   0)))
(define-test sacla-must-sequence.1674 (:tag :sacla)
 (assert-true
  (null
   (mismatch '((a b c) (b c d) (d e f))
             #((b b c) (c c d) (e e f))
             :key #'cdr
             :test #'equal))))
(define-test sacla-must-sequence.1675 (:tag :sacla)
 (assert-true
  (eql
   (mismatch '((a b c) (b c d) (d e f) (e f g))
             #((b b c) (c c d) (e e f))
             :key #'cdr
             :test #'equal)
   3)))
(define-test sacla-must-sequence.1676 (:tag :sacla)
 (assert-true
  (eql
   (mismatch '((a b c) (b c d) (d e f) (e f g))
             #((b b c) (c c d) (e e f))
             :key #'cdr
             :test #'equal
             :from-end t)
   4)))
(define-test sacla-must-sequence.1677 (:tag :sacla)
 (assert-true
  (eql
   (mismatch '((a a a) (a b c) (b c d) (d e f))
             #((b b c) (c c d) (e e f))
             :key #'cdr
             :test #'equal
             :from-end t)
   1)))
(define-test sacla-must-sequence.1678 (:tag :sacla)
 (assert-true
  (null
   (mismatch '((a a a) (a b c) (b c d) (d e f) (e f g))
             #((b b c) (c c d) (e e f))
             :key #'cdr
             :test #'equal
             :from-end t
             :start1 1
             :end1 4))))
(define-test sacla-must-sequence.1679 (:tag :sacla)
 (assert-true
  (eql
   (mismatch '((a a a) (a b c) (b c d) (d e f) (e f g))
             #((b b c) (c c d) (e e f))
             :key #'cdr
             :test #'equal
             :from-end t
             :start1 1)
   5)))
(define-test sacla-must-sequence.1680 (:tag :sacla)
 (assert-true
  (eql
   (mismatch '((a a a) (a b c) (b c d) (d e f) (e f g))
             #((b b c) (c c d) (e e f))
             :key #'cdr
             :test #'equal
             :from-end t
             :end1 3
             :start2 1
             :end2 2)
   2)))
(define-test sacla-must-sequence.1681 (:tag :sacla)
 (assert-true (null (mismatch #() #()))))
(define-test sacla-must-sequence.1682 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c) #(x y z)) 0)))
(define-test sacla-must-sequence.1683 (:tag :sacla)
 (assert-true (eql (mismatch #() #(x y z)) 0)))
(define-test sacla-must-sequence.1684 (:tag :sacla)
 (assert-true (eql (mismatch #(x y z) #()) 0)))
(define-test sacla-must-sequence.1685 (:tag :sacla)
 (assert-true (null (mismatch #(a) #(a)))))
(define-test sacla-must-sequence.1686 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c x y z) #(a b c)) 3)))
(define-test sacla-must-sequence.1687 (:tag :sacla)
 (assert-true (null (mismatch #(a b c) #(a b c)))))
(define-test sacla-must-sequence.1688 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c d e f) #(a b c)) 3)))
(define-test sacla-must-sequence.1689 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c) #(a b c d e f)) 3)))
(define-test sacla-must-sequence.1690 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c) #(a b x)) 2)))
(define-test sacla-must-sequence.1691 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c) #(a x c)) 1)))
(define-test sacla-must-sequence.1692 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c) #(x b c)) 0)))
(define-test sacla-must-sequence.1693 (:tag :sacla)
 (assert-true (eql (mismatch #(x y z a b c x y z) #(a b c) :start1 3) 6)))
(define-test sacla-must-sequence.1694 (:tag :sacla)
 (assert-true
  (eql (mismatch #(x y z a b c x y z) #(a b c) :start1 3 :end1 nil) 6)))
(define-test sacla-must-sequence.1695 (:tag :sacla)
 (assert-true
  (eql (mismatch #(x y z a b c x y z) #(a b c) :start1 3 :end1 4) 4)))
(define-test sacla-must-sequence.1696 (:tag :sacla)
 (assert-true
  (eql (mismatch #(x y z a b c x y z) #(a b c) :start1 3 :end1 3) 3)))
(define-test sacla-must-sequence.1697 (:tag :sacla)
 (assert-true (null (mismatch #(x y z) #() :start1 0 :end1 0))))
(define-test sacla-must-sequence.1698 (:tag :sacla)
 (assert-true (null (mismatch #(x y z) #() :start1 1 :end1 1))))
(define-test sacla-must-sequence.1699 (:tag :sacla)
 (assert-true (null (mismatch #(x y z) #() :start1 2 :end1 2))))
(define-test sacla-must-sequence.1700 (:tag :sacla)
 (assert-true (null (mismatch #(x y z) #() :start1 3 :end1 3))))
(define-test sacla-must-sequence.1701 (:tag :sacla)
 (assert-true
  (null (mismatch #(x y z) #() :start1 0 :end1 0 :start2 0 :end2 0))))
(define-test sacla-must-sequence.1702 (:tag :sacla)
 (assert-true
  (null (mismatch #(x y z) #() :start1 1 :end1 1 :start2 1 :end2 1))))
(define-test sacla-must-sequence.1703 (:tag :sacla)
 (assert-true
  (null (mismatch #(x y z) #() :start1 2 :end1 2 :start2 2 :end2 2))))
(define-test sacla-must-sequence.1704 (:tag :sacla)
 (assert-true
  (null (mismatch #(x y z) #() :start1 3 :end1 3 :start2 3 :end2 3))))
(define-test sacla-must-sequence.1705 (:tag :sacla)
 (assert-true
  (null (mismatch #(x y z) #() :start1 0 :end1 0 :start2 3 :end2 3))))
(define-test sacla-must-sequence.1706 (:tag :sacla)
 (assert-true
  (null (mismatch #(x y z) #() :start1 1 :end1 1 :start2 2 :end2 2))))
(define-test sacla-must-sequence.1707 (:tag :sacla)
 (assert-true
  (null (mismatch #(x y z) #() :start1 2 :end1 2 :start2 1 :end2 1))))
(define-test sacla-must-sequence.1708 (:tag :sacla)
 (assert-true
  (null (mismatch #(x y z) #() :start1 3 :end1 3 :start2 0 :end2 0))))
(define-test sacla-must-sequence.1709 (:tag :sacla)
 (assert-true (eql (mismatch #(x y z) #(a b c) :start1 0 :end1 0) 0)))
(define-test sacla-must-sequence.1710 (:tag :sacla)
 (assert-true (eql (mismatch #(x y z) #(a b c) :start1 1 :end1 1) 1)))
(define-test sacla-must-sequence.1711 (:tag :sacla)
 (assert-true (eql (mismatch #(x y z) #(a b c) :start1 2 :end1 2) 2)))
(define-test sacla-must-sequence.1712 (:tag :sacla)
 (assert-true (eql (mismatch #(x y z) #(a b c) :start1 3 :end1 3) 3)))
(define-test sacla-must-sequence.1713 (:tag :sacla)
 (assert-true (eql (mismatch #(x y z) #(x y z) :start1 0 :end1 1) 1)))
(define-test sacla-must-sequence.1714 (:tag :sacla)
 (assert-true (eql (mismatch #(x y z) #(x y z) :start1 0 :end1 2) 2)))
(define-test sacla-must-sequence.1715 (:tag :sacla)
 (assert-true (eql (mismatch #(x y z) #(x y z z) :start1 0 :end1 3) 3)))
(define-test sacla-must-sequence.1716 (:tag :sacla)
 (assert-true (null (mismatch #(x y z) #(x y z) :start1 0 :end1 3))))
(define-test sacla-must-sequence.1717 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c x y z) #(x y z a b c)) 0)))
(define-test sacla-must-sequence.1718 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c x y z) #(x y z a b c) :start1 3) 6)))
(define-test sacla-must-sequence.1719 (:tag :sacla)
 (assert-true
  (eql (mismatch #(a b c x y z a b c) #(x y z a b c x y z) :start1 3) 9)))
(define-test sacla-must-sequence.1720 (:tag :sacla)
 (assert-true
  (eql (mismatch #(a b c x y z a b c) #(x y z a b c x y z) :start1 6) 6)))
(define-test sacla-must-sequence.1721 (:tag :sacla)
 (assert-true
  (eql (mismatch #(a b c x y z a b c) #(x y z a b c x y z) :start1 6 :start2 3)
       9)))
(define-test sacla-must-sequence.1722 (:tag :sacla)
 (assert-true
  (eql (mismatch #(a b c x y z a b c) #(x y z a b c x y z) :start1 0 :start2 3)
       6)))
(define-test sacla-must-sequence.1723 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c) #(a b c x y z)) 3)))
(define-test sacla-must-sequence.1724 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c) #(x a b c y z)) 0)))
(define-test sacla-must-sequence.1725 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c) #(x a b c y z) :start2 1) 3)))
(define-test sacla-must-sequence.1726 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c) #(x a b c y z) :start2 1 :end2 nil) 3)))
(define-test sacla-must-sequence.1727 (:tag :sacla)
 (assert-true (null (mismatch #(a b c) #(x a b c y z) :start2 1 :end2 4))))
(define-test sacla-must-sequence.1728 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c d e) #(c d)) 0)))
(define-test sacla-must-sequence.1729 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c d e) #(c d) :start1 2) 4)))
(define-test sacla-must-sequence.1730 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c d e) #(c d) :start1 2 :end1 3) 3)))
(define-test sacla-must-sequence.1731 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c d e) #(c d) :start1 2 :start2 1) 2)))
(define-test sacla-must-sequence.1732 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c d e) #(c d) :start1 3 :start2 1) 4)))
(define-test sacla-must-sequence.1733 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c d e) #(c d) :start1 2 :end2 1) 3)))
(define-test sacla-must-sequence.1734 (:tag :sacla)
 (assert-true
  (null (mismatch #(a b c d) #(a b c d) :start1 1 :end1 2 :start2 1 :end2 2))))
(define-test sacla-must-sequence.1735 (:tag :sacla)
 (assert-true
  (null (mismatch #(a b c d) #(a b c d) :start1 1 :end1 3 :start2 1 :end2 3))))
(define-test sacla-must-sequence.1736 (:tag :sacla)
 (assert-true
  (null (mismatch #(a b c d) #(a b c d) :start1 1 :end1 4 :start2 1 :end2 4))))
(define-test sacla-must-sequence.1737 (:tag :sacla)
 (assert-true
  (eql (mismatch #(a b c d) #(a b c d) :start1 1 :end1 nil :start2 1 :end2 1)
       1)))
(define-test sacla-must-sequence.1738 (:tag :sacla)
 (assert-true
  (eql (mismatch #(a b c d) #(a b c d) :start1 1 :end1 nil :start2 1 :end2 2)
       2)))
(define-test sacla-must-sequence.1739 (:tag :sacla)
 (assert-true
  (eql (mismatch #(a b c d) #(a b c d) :start1 1 :end1 nil :start2 1 :end2 3)
       3)))
(define-test sacla-must-sequence.1740 (:tag :sacla)
 (assert-true
  (null
   (mismatch #(a b c d) #(a b c d) :start1 1 :end1 nil :start2 1 :end2 4))))
(define-test sacla-must-sequence.1741 (:tag :sacla)
 (assert-true
  (eql (mismatch #(a b c d) #(a b c d) :start1 1 :end1 1 :start2 1) 1)))
(define-test sacla-must-sequence.1742 (:tag :sacla)
 (assert-true
  (eql (mismatch #(a b c d) #(a b c d) :start1 1 :end1 2 :start2 1) 2)))
(define-test sacla-must-sequence.1743 (:tag :sacla)
 (assert-true
  (eql (mismatch #(a b c d) #(a b c d) :start1 1 :end1 3 :start2 1) 3)))
(define-test sacla-must-sequence.1744 (:tag :sacla)
 (assert-true
  (null (mismatch #(a b c d) #(a b c d) :start1 1 :end1 4 :start2 1))))
(define-test sacla-must-sequence.1745 (:tag :sacla)
 (assert-true (null (mismatch #(a b c) #(a b c) :from-end t))))
(define-test sacla-must-sequence.1746 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c d) #(a b c) :from-end t) 4)))
(define-test sacla-must-sequence.1747 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c) #(c) :from-end t) 2)))
(define-test sacla-must-sequence.1748 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c) #(z a b c) :from-end t) 0)))
(define-test sacla-must-sequence.1749 (:tag :sacla)
 (assert-true (eql (mismatch #(a b c) #(x y z a b c) :from-end t) 0)))
(define-test sacla-must-sequence.1750 (:tag :sacla)
 (assert-true (eql (mismatch #(x y z a b c) #(a b c) :from-end t) 3)))
(define-test sacla-must-sequence.1751 (:tag :sacla)
 (assert-true (eql (mismatch #(x y z a b c) #(a b c) :end1 3 :from-end t) 3)))
(define-test sacla-must-sequence.1752 (:tag :sacla)
 (assert-true (eql (mismatch #(x y z a b c) #(a b c) :end1 5 :from-end t) 5)))
(define-test sacla-must-sequence.1753 (:tag :sacla)
 (assert-true
  (eql (mismatch #(x y z a b c x y z) #(a b c) :end1 6 :from-end t) 3)))
(define-test sacla-must-sequence.1754 (:tag :sacla)
 (assert-true
  (eql (mismatch #(x y z a b c x y z) #(a b c) :start1 2 :end1 6 :from-end t)
       3)))
(define-test sacla-must-sequence.1755 (:tag :sacla)
 (assert-true
  (eql
   (mismatch #(x y z a b c x y z)
             #(a b c)
             :from-end t
             :start1 2
             :end1 5
             :start2 1
             :end2 2)
   4)))
(define-test sacla-must-sequence.1756 (:tag :sacla)
 (assert-true
  (eql
   (mismatch #(x y z a b c x y z) #(a b c) :start1 2 :end1 5 :start2 1 :end2 2)
   2)))
(define-test sacla-must-sequence.1757 (:tag :sacla)
 (assert-true (eql (mismatch #((a) (b) (c)) #((a) (b) (c))) 0)))
(define-test sacla-must-sequence.1758 (:tag :sacla)
 (assert-true (null (mismatch #((a) (b) (c)) #((a) (b) (c)) :key #'car))))
(define-test sacla-must-sequence.1759 (:tag :sacla)
 (assert-true (null (mismatch #((a) (b) (c)) #((a) (b) (c)) :test #'equal))))
(define-test sacla-must-sequence.1760 (:tag :sacla)
 (assert-true (eql (mismatch #(#(a) #(b) #(c)) #(#(a) #(b) #(c))) 0)))
(define-test sacla-must-sequence.1761 (:tag :sacla)
 (assert-true
  (null (mismatch #(#(a) #(b) #(c)) #(#(a) #(b) #(c)) :test #'equalp))))
(define-test sacla-must-sequence.1762 (:tag :sacla)
 (assert-true (eql (mismatch #((a) (b) (c) (d)) #((a) (b) (c)) :key #'car) 3)))
(define-test sacla-must-sequence.1763 (:tag :sacla)
 (assert-true (eql (mismatch #((a) (b) (c)) #((a) (b) (c) (d)) :key #'car) 3)))
(define-test sacla-must-sequence.1764 (:tag :sacla)
 (assert-true (eql (mismatch #(#\a #\b #\c) #(#\A #\B #\C)) 0)))
(define-test sacla-must-sequence.1765 (:tag :sacla)
 (assert-true
  (null (mismatch #(#\a #\b #\c) #(#\A #\B #\C) :key #'char-upcase))))
(define-test sacla-must-sequence.1766 (:tag :sacla)
 (assert-true
  (null (mismatch #(#\a #\b #\c) #(#\A #\B #\C) :key #'char-downcase))))
(define-test sacla-must-sequence.1767 (:tag :sacla)
 (assert-true
  (null
   (mismatch #(#\a #\b #\c)
             #(#\A #\B #\C)
             :key #'char-upcase
             :start1 1
             :end1 2
             :start2 1
             :end2 2))))
(define-test sacla-must-sequence.1768 (:tag :sacla)
 (assert-true
  (null
   (mismatch #(#\a #\b #\c)
             #(#\A #\B #\C)
             :key #'char-upcase
             :start1 2
             :start2 2))))
(define-test sacla-must-sequence.1769 (:tag :sacla)
 (assert-true
  (eql (mismatch #((a b c) (b c d) (d e f)) #((b b c) (c c d) (e e f))) 0)))
(define-test sacla-must-sequence.1770 (:tag :sacla)
 (assert-true
  (eql
   (mismatch #((a b c) (b c d) (d e f)) #((b b c) (c c d) (e e f)) :key #'cdr)
   0)))
(define-test sacla-must-sequence.1771 (:tag :sacla)
 (assert-true
  (null
   (mismatch #((a b c) (b c d) (d e f))
             #((b b c) (c c d) (e e f))
             :key #'cdr
             :test #'equal))))
(define-test sacla-must-sequence.1772 (:tag :sacla)
 (assert-true
  (eql
   (mismatch #((a b c) (b c d) (d e f) (e f g))
             #((b b c) (c c d) (e e f))
             :key #'cdr
             :test #'equal)
   3)))
(define-test sacla-must-sequence.1773 (:tag :sacla)
 (assert-true
  (eql
   (mismatch #((a b c) (b c d) (d e f) (e f g))
             #((b b c) (c c d) (e e f))
             :key #'cdr
             :test #'equal
             :from-end t)
   4)))
(define-test sacla-must-sequence.1774 (:tag :sacla)
 (assert-true
  (eql
   (mismatch #((a a a) (a b c) (b c d) (d e f))
             #((b b c) (c c d) (e e f))
             :key #'cdr
             :test #'equal
             :from-end t)
   1)))
(define-test sacla-must-sequence.1775 (:tag :sacla)
 (assert-true
  (null
   (mismatch #((a a a) (a b c) (b c d) (d e f) (e f g))
             #((b b c) (c c d) (e e f))
             :key #'cdr
             :test #'equal
             :from-end t
             :start1 1
             :end1 4))))
(define-test sacla-must-sequence.1776 (:tag :sacla)
 (assert-true
  (eql
   (mismatch #((a a a) (a b c) (b c d) (d e f) (e f g))
             #((b b c) (c c d) (e e f))
             :key #'cdr
             :test #'equal
             :from-end t
             :start1 1)
   5)))
(define-test sacla-must-sequence.1777 (:tag :sacla)
 (assert-true
  (eql
   (mismatch #((a a a) (a b c) (b c d) (d e f) (e f g))
             #((b b c) (c c d) (e e f))
             :key #'cdr
             :test #'equal
             :from-end t
             :end1 3
             :start2 1
             :end2 2)
   2)))
(define-test sacla-must-sequence.1778 (:tag :sacla)
 (assert-true (eql (mismatch "abc" "xyz") 0)))
(define-test sacla-must-sequence.1779 (:tag :sacla)
 (assert-true (null (mismatch "" ""))))
(define-test sacla-must-sequence.1780 (:tag :sacla)
 (assert-true (null (mismatch "a" "a"))))
(define-test sacla-must-sequence.1781 (:tag :sacla)
 (assert-true (null (mismatch "abc" "abc"))))
(define-test sacla-must-sequence.1782 (:tag :sacla)
 (assert-true (null (mismatch "abc" "ABC" :key #'char-downcase))))
(define-test sacla-must-sequence.1783 (:tag :sacla)
 (assert-true (null (mismatch "abc" "ABC" :test #'char-equal))))
(define-test sacla-must-sequence.1784 (:tag :sacla)
 (assert-true (eql (mismatch "abcde" "abc") 3)))
(define-test sacla-must-sequence.1785 (:tag :sacla)
 (assert-true (eql (mismatch "abc" "abcde") 3)))
(define-test sacla-must-sequence.1786 (:tag :sacla)
 (assert-true (eql (mismatch "abc" "abxyz") 2)))
(define-test sacla-must-sequence.1787 (:tag :sacla)
 (assert-true (eql (mismatch "abcde" "abx") 2)))
(define-test sacla-must-sequence.1788 (:tag :sacla)
 (assert-true (null (mismatch "abc" "abc" :from-end t))))
(define-test sacla-must-sequence.1789 (:tag :sacla)
 (assert-true (eql (mismatch "abcxyz" "xyzxyz" :from-end t) 3)))
(define-test sacla-must-sequence.1790 (:tag :sacla)
 (assert-true (eql (mismatch "abcxyz" "xyz" :from-end t) 3)))
(define-test sacla-must-sequence.1791 (:tag :sacla)
 (assert-true (eql (mismatch "xyz" "abcxyz" :from-end t) 0)))
(define-test sacla-must-sequence.1792 (:tag :sacla)
 (assert-true (eql (mismatch "ayz" "abcxyz" :from-end t) 1)))
(define-test sacla-must-sequence.1793 (:tag :sacla)
 (assert-true (null (mismatch "abc" "xyz" :test #'char<))))
(define-test sacla-must-sequence.1794 (:tag :sacla)
 (assert-true (eql (mismatch "abc" "xyz" :test #'char>) 0)))
(define-test sacla-must-sequence.1795 (:tag :sacla)
 (assert-true (eql (mismatch "abcxyz" "abcdefg") 3)))
(define-test sacla-must-sequence.1796 (:tag :sacla)
 (assert-true (eql (mismatch "1xyz" "22xyz" :from-end t) 1)))
(define-test sacla-must-sequence.1797 (:tag :sacla)
 (assert-true (null (mismatch #*010101 #*010101))))
(define-test sacla-must-sequence.1798 (:tag :sacla)
 (assert-true (eql (mismatch #*010 #*101) 0)))
(define-test sacla-must-sequence.1799 (:tag :sacla)
 (assert-true (eql (mismatch #*010 #*101 :from-end t) 3)))
(define-test sacla-must-sequence.1800 (:tag :sacla)
 (assert-true (eql (mismatch #*0101 #*010101) 4)))
(define-test sacla-must-sequence.1801 (:tag :sacla)
 (assert-true (eql (mismatch #*010101 #*0101) 4)))
(define-test sacla-must-sequence.1802 (:tag :sacla)
 (assert-true (eql (mismatch #*010100 #*010111) 4)))
(define-test sacla-must-sequence.1803 (:tag :sacla)
 (assert-true (null (mismatch #*0101 #*0101 :from-end t))))
(define-test sacla-must-sequence.1804 (:tag :sacla)
 (assert-true (eql (mismatch #*00101 #*0101 :from-end t) 1)))
(define-test sacla-must-sequence.1805 (:tag :sacla)
 (assert-true (eql (mismatch #*0101 #*00101 :from-end t) 0)))
(define-test sacla-must-sequence.1806 (:tag :sacla)
 (assert-true (eql (mismatch #*00101 #*10101 :from-end t) 1)))
(define-test sacla-must-sequence.1807 (:tag :sacla)
 (assert-true
  (equal (replace "abcdefghij" "0123456789" :start1 4 :end1 7 :start2 4)
         "abcd456hij")))
(define-test sacla-must-sequence.1808 (:tag :sacla)
 (assert-true
  (let ((lst (copy-seq "012345678")))
    (and (equal (replace lst lst :start1 2 :start2 0) "010123456")
         (equal lst "010123456")))))
(define-test sacla-must-sequence.1809 (:tag :sacla)
 (assert-true
  (let* ((list0 (list 'a 'b 'c 'd 'e)) (list (replace list0 '(x y z))))
    (and (eq list0 list) (equal list0 '(x y z d e))))))
(define-test sacla-must-sequence.1810 (:tag :sacla)
 (assert-true
  (let* ((list0 (list 'a 'b 'c 'd 'e))
         (list (replace list0 '(x y z) :start1 1)))
    (and (eq list0 list) (equal list0 '(a x y z e))))))
(define-test sacla-must-sequence.1811 (:tag :sacla)
 (assert-true
  (let* ((list0 (list 'a 'b 'c 'd 'e))
         (list (replace list0 '(x y z) :start1 1 :end1 nil)))
    (and (eq list0 list) (equal list0 '(a x y z e))))))
(define-test sacla-must-sequence.1812 (:tag :sacla)
 (assert-true
  (let* ((list0 (list 'a 'b 'c 'd 'e))
         (list (replace list0 '(x y z) :start1 1 :start2 1)))
    (and (eq list0 list) (equal list0 '(a y z d e))))))
(define-test sacla-must-sequence.1813 (:tag :sacla)
 (assert-true
  (let* ((list0 (list 'a 'b 'c 'd 'e))
         (list (replace list0 '(x y z) :start1 1 :start2 1 :end2 nil)))
    (and (eq list0 list) (equal list0 '(a y z d e))))))
(define-test sacla-must-sequence.1814 (:tag :sacla)
 (assert-true
  (let* ((list0 (list 'a 'b 'c 'd 'e))
         (list
          (replace list0 '(x y z) :start1 1 :end1 nil :start2 1 :end2 nil)))
    (and (eq list0 list) (equal list0 '(a y z d e))))))
(define-test sacla-must-sequence.1815 (:tag :sacla)
 (assert-true
  (let* ((list0 (list 'a 'b 'c 'd 'e))
         (list (replace list0 '(x y z) :start1 1 :end1 2 :start2 1)))
    (and (eq list0 list) (equal list0 '(a y c d e))))))
(define-test sacla-must-sequence.1816 (:tag :sacla)
 (assert-true
  (let* ((list0 (list 'a 'b 'c 'd 'e))
         (list (replace list0 '(x y z) :start1 1 :end1 1)))
    (and (eq list0 list) (equal list0 '(a b c d e))))))
(define-test sacla-must-sequence.1817 (:tag :sacla)
 (assert-true
  (let* ((list0 (list 'a 'b 'c 'd 'e))
         (list (replace list0 '(x y z) :start1 2 :end1 2)))
    (and (eq list0 list) (equal list0 '(a b c d e))))))
(define-test sacla-must-sequence.1818 (:tag :sacla)
 (assert-true
  (let* ((list0 (list 'a 'b 'c 'd 'e))
         (list (replace list0 '(x y z) :start1 3 :end1 3)))
    (and (eq list0 list) (equal list0 '(a b c d e))))))
(define-test sacla-must-sequence.1819 (:tag :sacla)
 (assert-true
  (let* ((list0 (list 'a 'b 'c 'd 'e))
         (list (replace list0 '(x y z) :start1 4 :end1 4)))
    (and (eq list0 list) (equal list0 '(a b c d e))))))
(define-test sacla-must-sequence.1820 (:tag :sacla)
 (assert-true
  (let* ((list0 (list 'a 'b 'c 'd 'e))
         (list (replace list0 '(x y z) :start1 5 :end1 5)))
    (and (eq list0 list) (equal list0 '(a b c d e))))))
(define-test sacla-must-sequence.1821 (:tag :sacla)
 (assert-true (null (replace nil nil))))
(define-test sacla-must-sequence.1822 (:tag :sacla)
 (assert-true (null (replace nil '(a b c)))))
(define-test sacla-must-sequence.1823 (:tag :sacla)
 (assert-true
  (let* ((list0 (list 'a 'b 'c)) (list (replace list0 'nil)))
    (and (eq list0 list) (equal list0 '(a b c))))))
(define-test sacla-must-sequence.1824 (:tag :sacla)
 (assert-true
  (let* ((list0 (list 'a 'b 'c 'd 'e)) (list (replace list0 list0)))
    (and (eq list0 list) (equal list0 '(a b c d e))))))
(define-test sacla-must-sequence.1825 (:tag :sacla)
 (assert-true
  (let* ((list0 (list 'a 'b 'c 'd 'e)) (list (replace list0 list0 :start1 3)))
    (and (eq list0 list) (equal list0 '(a b c a b))))))
(define-test sacla-must-sequence.1826 (:tag :sacla)
 (assert-true
  (let* ((list0 (list 'a 'b 'c 'd 'e)) (list (replace list0 list0 :start1 1)))
    (and (eq list0 list) (equal list0 '(a a b c d))))))
(define-test sacla-must-sequence.1827 (:tag :sacla)
 (assert-true
  (let* ((list0 (list 'a 'b 'c 'd 'e))
         (list (replace list0 list0 :start1 1 :end1 3)))
    (and (eq list0 list) (equal list0 '(a a b d e))))))
(define-test sacla-must-sequence.1828 (:tag :sacla)
 (assert-true
  (let* ((list0 (list 'a 'b 'c)) (list (replace list0 '(x y z))))
    (and (eq list0 list) (equal list0 '(x y z))))))
(define-test sacla-must-sequence.1829 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c 'd 'e)) (vector (replace vector0 '(x y z))))
    (and (eq vector0 vector) (equalp vector0 #(x y z d e))))))
(define-test sacla-must-sequence.1830 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c 'd 'e))
         (vector (replace vector0 '(x y z) :start1 1)))
    (and (eq vector0 vector) (equalp vector0 #(a x y z e))))))
(define-test sacla-must-sequence.1831 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c 'd 'e))
         (vector (replace vector0 '(x y z) :start1 1 :end1 nil)))
    (and (eq vector0 vector) (equalp vector0 #(a x y z e))))))
(define-test sacla-must-sequence.1832 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c 'd 'e))
         (vector (replace vector0 '(x y z) :start1 1 :start2 1)))
    (and (eq vector0 vector) (equalp vector0 #(a y z d e))))))
(define-test sacla-must-sequence.1833 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c 'd 'e))
         (vector (replace vector0 '(x y z) :start1 1 :start2 1 :end2 nil)))
    (and (eq vector0 vector) (equalp vector0 #(a y z d e))))))
(define-test sacla-must-sequence.1834 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c 'd 'e))
         (vector
          (replace vector0 '(x y z) :start1 1 :end1 nil :start2 1 :end2 nil)))
    (and (eq vector0 vector) (equalp vector0 #(a y z d e))))))
(define-test sacla-must-sequence.1835 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c 'd 'e))
         (vector (replace vector0 '(x y z) :start1 1 :end1 2 :start2 1)))
    (and (eq vector0 vector) (equalp vector0 #(a y c d e))))))
(define-test sacla-must-sequence.1836 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c 'd 'e))
         (vector (replace vector0 '(x y z) :start1 1 :end1 1)))
    (and (eq vector0 vector) (equalp vector0 #(a b c d e))))))
(define-test sacla-must-sequence.1837 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c 'd 'e))
         (vector (replace vector0 '(x y z) :start1 2 :end1 2)))
    (and (eq vector0 vector) (equalp vector0 #(a b c d e))))))
(define-test sacla-must-sequence.1838 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c 'd 'e))
         (vector (replace vector0 '(x y z) :start1 3 :end1 3)))
    (and (eq vector0 vector) (equalp vector0 #(a b c d e))))))
(define-test sacla-must-sequence.1839 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c 'd 'e))
         (vector (replace vector0 '(x y z) :start1 4 :end1 4)))
    (and (eq vector0 vector) (equalp vector0 #(a b c d e))))))
(define-test sacla-must-sequence.1840 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c 'd 'e))
         (vector (replace vector0 '(x y z) :start1 5 :end1 5)))
    (and (eq vector0 vector) (equalp vector0 #(a b c d e))))))
(define-test sacla-must-sequence.1841 (:tag :sacla)
 (assert-true (null (replace nil #()))))
(define-test sacla-must-sequence.1842 (:tag :sacla)
 (assert-true (null (replace nil #(a b c)))))
(define-test sacla-must-sequence.1843 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c)) (vector (replace vector0 'nil)))
    (and (eq vector0 vector) (equalp vector0 #(a b c))))))
(define-test sacla-must-sequence.1844 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c 'd 'e)) (vector (replace vector0 vector0)))
    (and (eq vector0 vector) (equalp vector0 #(a b c d e))))))
(define-test sacla-must-sequence.1845 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c 'd 'e))
         (vector (replace vector0 vector0 :start1 3)))
    (and (eq vector0 vector) (equalp vector0 #(a b c a b))))))
(define-test sacla-must-sequence.1846 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c 'd 'e))
         (vector (replace vector0 vector0 :start1 1)))
    (and (eq vector0 vector) (equalp vector0 #(a a b c d))))))
(define-test sacla-must-sequence.1847 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c 'd 'e))
         (vector (replace vector0 vector0 :start1 1 :end1 3)))
    (and (eq vector0 vector) (equalp vector0 #(a a b d e))))))
(define-test sacla-must-sequence.1848 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c)) (vector (replace vector0 '(x y z))))
    (and (eq vector0 vector) (equalp vector0 #(x y z))))))
(define-test sacla-must-sequence.1849 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c 'd 'e)) (vector (replace vector0 #(x y z))))
    (and (eq vector0 vector) (equalp vector0 #(x y z d e))))))
(define-test sacla-must-sequence.1850 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c 'd 'e))
         (vector (replace vector0 #(x y z) :start1 1)))
    (and (eq vector0 vector) (equalp vector0 #(a x y z e))))))
(define-test sacla-must-sequence.1851 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c 'd 'e))
         (vector (replace vector0 #(x y z) :start1 1 :end1 nil)))
    (and (eq vector0 vector) (equalp vector0 #(a x y z e))))))
(define-test sacla-must-sequence.1852 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c 'd 'e))
         (vector (replace vector0 #(x y z) :start1 1 :start2 1)))
    (and (eq vector0 vector) (equalp vector0 #(a y z d e))))))
(define-test sacla-must-sequence.1853 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c 'd 'e))
         (vector (replace vector0 #(x y z) :start1 1 :start2 1 :end2 nil)))
    (and (eq vector0 vector) (equalp vector0 #(a y z d e))))))
(define-test sacla-must-sequence.1854 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c 'd 'e))
         (vector
          (replace vector0 #(x y z) :start1 1 :end1 nil :start2 1 :end2 nil)))
    (and (eq vector0 vector) (equalp vector0 #(a y z d e))))))
(define-test sacla-must-sequence.1855 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c 'd 'e))
         (vector (replace vector0 #(x y z) :start1 1 :end1 2 :start2 1)))
    (and (eq vector0 vector) (equalp vector0 #(a y c d e))))))
(define-test sacla-must-sequence.1856 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c 'd 'e))
         (vector (replace vector0 #(x y z) :start1 1 :end1 1)))
    (and (eq vector0 vector) (equalp vector0 #(a b c d e))))))
(define-test sacla-must-sequence.1857 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c 'd 'e))
         (vector (replace vector0 #(x y z) :start1 2 :end1 2)))
    (and (eq vector0 vector) (equalp vector0 #(a b c d e))))))
(define-test sacla-must-sequence.1858 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c 'd 'e))
         (vector (replace vector0 #(x y z) :start1 3 :end1 3)))
    (and (eq vector0 vector) (equalp vector0 #(a b c d e))))))
(define-test sacla-must-sequence.1859 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c 'd 'e))
         (vector (replace vector0 #(x y z) :start1 4 :end1 4)))
    (and (eq vector0 vector) (equalp vector0 #(a b c d e))))))
(define-test sacla-must-sequence.1860 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c 'd 'e))
         (vector (replace vector0 #(x y z) :start1 5 :end1 5)))
    (and (eq vector0 vector) (equalp vector0 #(a b c d e))))))
(define-test sacla-must-sequence.1861 (:tag :sacla)
 (assert-true (null (replace nil #()))))
(define-test sacla-must-sequence.1862 (:tag :sacla)
 (assert-true (null (replace nil #(a b c)))))
(define-test sacla-must-sequence.1863 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c)) (vector (replace vector0 #())))
    (and (eq vector0 vector) (equalp vector0 #(a b c))))))
(define-test sacla-must-sequence.1864 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c 'd 'e)) (vector (replace vector0 vector0)))
    (and (eq vector0 vector) (equalp vector0 #(a b c d e))))))
(define-test sacla-must-sequence.1865 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c 'd 'e))
         (vector (replace vector0 vector0 :start1 3)))
    (and (eq vector0 vector) (equalp vector0 #(a b c a b))))))
(define-test sacla-must-sequence.1866 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c 'd 'e))
         (vector (replace vector0 vector0 :start1 1)))
    (and (eq vector0 vector) (equalp vector0 #(a a b c d))))))
(define-test sacla-must-sequence.1867 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c 'd 'e))
         (vector (replace vector0 vector0 :start1 1 :end1 3)))
    (and (eq vector0 vector) (equalp vector0 #(a a b d e))))))
(define-test sacla-must-sequence.1868 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 'a 'b 'c)) (vector (replace vector0 #(x y z))))
    (and (eq vector0 vector) (equalp vector0 #(x y z))))))
(define-test sacla-must-sequence.1869 (:tag :sacla)
 (assert-true
  (let* ((str0 (copy-seq "abc")) (str (replace str0 "xyz")))
    (and (eq str0 str) (equalp str0 "xyz")))))
(define-test sacla-must-sequence.1870 (:tag :sacla)
 (assert-true
  (let* ((str0 (copy-seq "")) (str (replace str0 "")))
    (and (eq str0 str) (equalp str0 "")))))
(define-test sacla-must-sequence.1871 (:tag :sacla)
 (assert-true
  (let* ((str0 (copy-seq "")) (str (replace str0 "xyz")))
    (and (eq str0 str) (equalp str0 "")))))
(define-test sacla-must-sequence.1872 (:tag :sacla)
 (assert-true
  (let* ((str0 (copy-seq "abc")) (str (replace str0 "")))
    (and (eq str0 str) (equalp str0 "abc")))))
(define-test sacla-must-sequence.1873 (:tag :sacla)
 (assert-true
  (let* ((str0 (copy-seq "abcdef")) (str (replace str0 "xyz" :start1 3)))
    (and (eq str0 str) (equalp str0 "abcxyz")))))
(define-test sacla-must-sequence.1874 (:tag :sacla)
 (assert-true
  (let* ((str0 (copy-seq "abcdef"))
         (str (replace str0 "xyz" :start1 4 :start2 1)))
    (and (eq str0 str) (equalp str0 "abcdyz")))))
(define-test sacla-must-sequence.1875 (:tag :sacla)
 (assert-true
  (let* ((str0 (copy-seq "abcdef"))
         (str (replace str0 "xyz" :start1 1 :end1 2 :start2 1)))
    (and (eq str0 str) (equalp str0 "aycdef")))))
(define-test sacla-must-sequence.1876 (:tag :sacla)
 (assert-true
  (let* ((str0 (copy-seq "abcdef"))
         (str (replace str0 "xyz" :start1 1 :start2 1 :end2 2)))
    (and (eq str0 str) (equalp str0 "aycdef")))))
(define-test sacla-must-sequence.1877 (:tag :sacla)
 (assert-true
  (let* ((str0 (copy-seq "abcdef")) (str (replace str0 str0 :start1 1)))
    (and (eq str0 str) (equalp str0 "aabcde")))))
(define-test sacla-must-sequence.1878 (:tag :sacla)
 (assert-true
  (let* ((bv0 (copy-seq #*0000)) (bv (replace bv0 #*1010)))
    (and (eq bv0 bv) (equalp bv0 #*1010)))))
(define-test sacla-must-sequence.1879 (:tag :sacla)
 (assert-true
  (let* ((bv0 (copy-seq #*)) (bv (replace bv0 #*1010)))
    (and (eq bv0 bv) (equalp bv0 #*)))))
(define-test sacla-must-sequence.1880 (:tag :sacla)
 (assert-true
  (let* ((bv0 (copy-seq #*0000)) (bv (replace bv0 #*)))
    (and (eq bv0 bv) (equalp bv0 #*0000)))))
(define-test sacla-must-sequence.1881 (:tag :sacla)
 (assert-true
  (let* ((bv0 (copy-seq #*0000)) (bv (replace bv0 #*1111 :start1 2)))
    (and (eq bv0 bv) (equalp bv0 #*0011)))))
(define-test sacla-must-sequence.1882 (:tag :sacla)
 (assert-true
  (let* ((bv0 (copy-seq #*1001))
         (bv (replace bv0 #*0110 :start1 1 :end1 3 :start2 1 :end2 3)))
    (and (eq bv0 bv) (equalp bv0 #*1111)))))
(define-test sacla-must-sequence.1883 (:tag :sacla)
 (assert-true
  (let* ((bv0 (copy-seq #*1010)) (bv (replace bv0 bv0 :start1 1)))
    (and (eq bv0 bv) (equalp bv0 #*1101)))))
(define-test sacla-must-sequence.1884 (:tag :sacla)
 (assert-true (equal (substitute #\. #\  "0 2 4 6") "0.2.4.6")))
(define-test sacla-must-sequence.1885 (:tag :sacla)
 (assert-true (equal (substitute 9 4 '(1 2 4 1 3 4 5)) '(1 2 9 1 3 9 5))))
(define-test sacla-must-sequence.1886 (:tag :sacla)
 (assert-true
  (equal (substitute 9 4 '(1 2 4 1 3 4 5) :count 1) '(1 2 9 1 3 4 5))))
(define-test sacla-must-sequence.1887 (:tag :sacla)
 (assert-true
  (equal (substitute 9 4 '(1 2 4 1 3 4 5) :count 1 :from-end t)
         '(1 2 4 1 3 9 5))))
(define-test sacla-must-sequence.1888 (:tag :sacla)
 (assert-true
  (equal (substitute 9 3 '(1 2 4 1 3 4 5) :test #'>) '(9 9 4 9 3 4 5))))
(define-test sacla-must-sequence.1889 (:tag :sacla)
 (assert-true
  (equal (substitute-if 0 #'evenp '((1) (2) (3) (4)) :start 2 :key #'car)
         '((1) (2) (3) 0))))
(define-test sacla-must-sequence.1890 (:tag :sacla)
 (assert-true
  (equal (substitute-if 9 #'oddp '(1 2 4 1 3 4 5)) '(9 2 4 9 9 4 9))))
(define-test sacla-must-sequence.1891 (:tag :sacla)
 (assert-true
  (equal (substitute-if 9 #'evenp '(1 2 4 1 3 4 5) :count 1 :from-end t)
         '(1 2 4 1 3 9 5))))
(define-test sacla-must-sequence.1892 (:tag :sacla)
 (assert-true
  (let ((some-things (list 'a 'car 'b 'cdr 'c)))
    (and
     (equal
      (nsubstitute-if "function was here"
                      #'fboundp
                      some-things
                      :count 1
                      :from-end t)
      '(a car b "function was here" c))
     (equal some-things '(a car b "function was here" c))))))
(define-test sacla-must-sequence.1893 (:tag :sacla)
 (assert-true
  (let ((alpha-tester (copy-seq "ab ")))
    (and (equal (nsubstitute-if-not #\z #'alpha-char-p alpha-tester) "abz")
         (equal alpha-tester "abz")))))
(define-test sacla-must-sequence.1894 (:tag :sacla)
 (assert-true (equal (substitute 'a 'x '(x y z)) '(a y z))))
(define-test sacla-must-sequence.1895 (:tag :sacla)
 (assert-true (equal (substitute 'b 'y '(x y z)) '(x b z))))
(define-test sacla-must-sequence.1896 (:tag :sacla)
 (assert-true (equal (substitute 'c 'z '(x y z)) '(x y c))))
(define-test sacla-must-sequence.1897 (:tag :sacla)
 (assert-true (equal (substitute 'a 'p '(x y z)) '(x y z))))
(define-test sacla-must-sequence.1898 (:tag :sacla)
 (assert-true (equal (substitute 'a 'x 'nil) 'nil)))
(define-test sacla-must-sequence.1899 (:tag :sacla)
 (assert-true
  (equal (substitute #\x #\b '(#\a #\b #\c #\d #\e) :test #'char<)
         '(#\a #\b #\x #\x #\x))))
(define-test sacla-must-sequence.1900 (:tag :sacla)
 (assert-true
  (equal
   (substitute #\x #\b '(#\a #\b #\c #\d #\e) :test-not (complement #'char<))
   '(#\a #\b #\x #\x #\x))))
(define-test sacla-must-sequence.1901 (:tag :sacla)
 (assert-true
  (equal (substitute '(a) 'x '((x) (y) (z)) :key #'car) '((a) (y) (z)))))
(define-test sacla-must-sequence.1902 (:tag :sacla)
 (assert-true (equal (substitute 'c 'b '(a b a b a b a b)) '(a c a c a c a c))))
(define-test sacla-must-sequence.1903 (:tag :sacla)
 (assert-true (equal (substitute 'a 'b '(b b b)) '(a a a))))
(define-test sacla-must-sequence.1904 (:tag :sacla)
 (assert-true
  (equal (substitute 'z 'x '(a x b x c x d x e x f)) '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.1905 (:tag :sacla)
 (assert-true
  (equal (substitute 'z 'x '(a x b x c x d x e x f) :count nil)
         '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.1906 (:tag :sacla)
 (assert-true
  (equal (substitute 'z 'x '(a x b x c x d x e x f) :count 0)
         '(a x b x c x d x e x f))))
(define-test sacla-must-sequence.1907 (:tag :sacla)
 (assert-true
  (equal (substitute 'z 'x '(a x b x c x d x e x f) :count -100)
         '(a x b x c x d x e x f))))
(define-test sacla-must-sequence.1908 (:tag :sacla)
 (assert-true
  (equal (substitute 'z 'x '(a x b x c x d x e x f) :count 1)
         '(a z b x c x d x e x f))))
(define-test sacla-must-sequence.1909 (:tag :sacla)
 (assert-true
  (equal (substitute 'z 'x '(a x b x c x d x e x f) :count 2)
         '(a z b z c x d x e x f))))
(define-test sacla-must-sequence.1910 (:tag :sacla)
 (assert-true
  (equal (substitute 'z 'x '(a x b x c x d x e x f) :count 3)
         '(a z b z c z d x e x f))))
(define-test sacla-must-sequence.1911 (:tag :sacla)
 (assert-true
  (equal (substitute 'z 'x '(a x b x c x d x e x f) :count 4)
         '(a z b z c z d z e x f))))
(define-test sacla-must-sequence.1912 (:tag :sacla)
 (assert-true
  (equal (substitute 'z 'x '(a x b x c x d x e x f) :count 5)
         '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.1913 (:tag :sacla)
 (assert-true
  (equal (substitute 'z 'x '(a x b x c x d x e x f) :count 6)
         '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.1914 (:tag :sacla)
 (assert-true
  (equal (substitute 'z 'x '(a x b x c x d x e x f) :count 7)
         '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.1915 (:tag :sacla)
 (assert-true
  (equal (substitute 'z 'x '(a x b x c x d x e x f) :count nil :from-end t)
         '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.1916 (:tag :sacla)
 (assert-true
  (equal (substitute 'z 'x '(a x b x c x d x e x f) :count 0 :from-end t)
         '(a x b x c x d x e x f))))
(define-test sacla-must-sequence.1917 (:tag :sacla)
 (assert-true
  (equal (substitute 'z 'x '(a x b x c x d x e x f) :count -100 :from-end t)
         '(a x b x c x d x e x f))))
(define-test sacla-must-sequence.1918 (:tag :sacla)
 (assert-true
  (equal (substitute 'z 'x '(a x b x c x d x e x f) :count 1 :from-end t)
         '(a x b x c x d x e z f))))
(define-test sacla-must-sequence.1919 (:tag :sacla)
 (assert-true
  (equal (substitute 'z 'x '(a x b x c x d x e x f) :count 2 :from-end t)
         '(a x b x c x d z e z f))))
(define-test sacla-must-sequence.1920 (:tag :sacla)
 (assert-true
  (equal (substitute 'z 'x '(a x b x c x d x e x f) :count 3 :from-end t)
         '(a x b x c z d z e z f))))
(define-test sacla-must-sequence.1921 (:tag :sacla)
 (assert-true
  (equal (substitute 'z 'x '(a x b x c x d x e x f) :count 4 :from-end t)
         '(a x b z c z d z e z f))))
(define-test sacla-must-sequence.1922 (:tag :sacla)
 (assert-true
  (equal (substitute 'z 'x '(a x b x c x d x e x f) :count 5 :from-end t)
         '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.1923 (:tag :sacla)
 (assert-true
  (equal (substitute 'z 'x '(a x b x c x d x e x f) :count 6 :from-end t)
         '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.1924 (:tag :sacla)
 (assert-true
  (equal (substitute 'z 'x '(a x b x c x d x e x f) :count 7 :from-end t)
         '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.1925 (:tag :sacla)
 (assert-true
  (equal (substitute 'z 'x '(a x b x c x d x e x f) :start 2 :count 1)
         '(a x b z c x d x e x f))))
(define-test sacla-must-sequence.1926 (:tag :sacla)
 (assert-true
  (equal (substitute 'z 'x '(a x b x c x d x e x f) :start 2 :end nil :count 1)
         '(a x b z c x d x e x f))))
(define-test sacla-must-sequence.1927 (:tag :sacla)
 (assert-true
  (equal (substitute 'z 'x '(a x b x c x d x e x f) :start 2 :end 6 :count 100)
         '(a x b z c z d x e x f))))
(define-test sacla-must-sequence.1928 (:tag :sacla)
 (assert-true
  (equal
   (substitute 'z 'x '(a x b x c x d x e x f) :start 2 :end 11 :count 100)
   '(a x b z c z d z e z f))))
(define-test sacla-must-sequence.1929 (:tag :sacla)
 (assert-true
  (equal (substitute 'z 'x '(a x b x c x d x e x f) :start 2 :end 8 :count 10)
         '(a x b z c z d z e x f))))
(define-test sacla-must-sequence.1930 (:tag :sacla)
 (assert-true
  (equal
   (substitute 'z
               'x
               '(a x b x c x d x e x f)
               :start 2
               :end 8
               :count 2
               :from-end t)
   '(a x b x c z d z e x f))))
(define-test sacla-must-sequence.1931 (:tag :sacla)
 (assert-true
  (equal (substitute #\z #\c '(#\a #\b #\c #\d #\e #\f) :test #'char<)
         '(#\a #\b #\c #\z #\z #\z))))
(define-test sacla-must-sequence.1932 (:tag :sacla)
 (assert-true
  (equal
   (substitute #\z
               #\c
               '(#\a #\b #\c #\d #\e #\f)
               :test-not (complement #'char<))
   '(#\a #\b #\c #\z #\z #\z))))
(define-test sacla-must-sequence.1933 (:tag :sacla)
 (assert-true
  (equal
   (substitute "peace" "war" '("love" "hate" "war" "peace") :test #'equal)
   '("love" "hate" "peace" "peace"))))
(define-test sacla-must-sequence.1934 (:tag :sacla)
 (assert-true
  (equal
   (substitute "peace"
               "war"
               '("love" "hate" "war" "peace")
               :test-not (complement #'equal))
   '("love" "hate" "peace" "peace"))))
(define-test sacla-must-sequence.1935 (:tag :sacla)
 (assert-true
  (equal
   (substitute "peace" "war" '("war" "War" "WAr" "WAR") :test #'string-equal)
   '("peace" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.1936 (:tag :sacla)
 (assert-true
  (equal
   (substitute "peace"
               "war"
               '("war" "War" "WAr" "WAR")
               :test-not (complement #'string-equal))
   '("peace" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.1937 (:tag :sacla)
 (assert-true
  (equal (substitute "peace" "WAR" '("war" "War" "WAr" "WAR") :test #'string=)
         '("war" "War" "WAr" "peace"))))
(define-test sacla-must-sequence.1938 (:tag :sacla)
 (assert-true
  (equal
   (substitute "peace"
               "WAR"
               '("war" "War" "WAr" "WAR")
               :test-not (complement #'string=))
   '("war" "War" "WAr" "peace"))))
(define-test sacla-must-sequence.1939 (:tag :sacla)
 (assert-true
  (equal
   (substitute "peace"
               "WAR"
               '("war" "War" "WAr" "WAR")
               :test #'string=
               :key #'string-upcase)
   '("peace" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.1940 (:tag :sacla)
 (assert-true
  (equal
   (substitute "peace"
               "WAR"
               '("war" "War" "WAr" "WAR")
               :test-not (complement #'string=)
               :key #'string-upcase)
   '("peace" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.1941 (:tag :sacla)
 (assert-true
  (equal
   (substitute "peace"
               "WAR"
               '("war" "War" "WAr" "WAR")
               :start 1
               :end 2
               :test #'string=
               :key #'string-upcase)
   '("war" "peace" "WAr" "WAR"))))
(define-test sacla-must-sequence.1942 (:tag :sacla)
 (assert-true
  (equal
   (substitute "peace"
               "WAR"
               '("war" "War" "WAr" "WAR")
               :start 1
               :end 2
               :test-not (complement #'string=)
               :key #'string-upcase)
   '("war" "peace" "WAr" "WAR"))))
(define-test sacla-must-sequence.1943 (:tag :sacla)
 (assert-true
  (equal
   (substitute "peace"
               "WAR"
               '("war" "War" "WAr" "WAR")
               :start 1
               :end nil
               :test #'string=
               :key #'string-upcase)
   '("war" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.1944 (:tag :sacla)
 (assert-true
  (equal
   (substitute "peace"
               "WAR"
               '("war" "War" "WAr" "WAR")
               :start 1
               :end nil
               :test-not (complement #'string=)
               :key #'string-upcase)
   '("war" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.1945 (:tag :sacla)
 (assert-true
  (equal
   (substitute "peace"
               "war"
               '("war" "War" "WAr" "WAR")
               :test #'string=
               :key #'string-upcase)
   '("war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.1946 (:tag :sacla)
 (assert-true
  (equal
   (substitute "peace"
               "war"
               '("war" "War" "WAr" "WAR")
               :test-not (complement #'string=)
               :key #'string-upcase)
   '("war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.1947 (:tag :sacla)
 (assert-true
  (equal
   (substitute "peace"
               "WAR"
               '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
               :start 1
               :end 7
               :count 1
               :test-not (complement #'string=)
               :key #'string-upcase)
   '("war" "peace" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.1948 (:tag :sacla)
 (assert-true
  (equal
   (substitute "peace"
               "WAR"
               '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
               :start 1
               :end 7
               :count 2
               :test-not (complement #'string=)
               :key #'string-upcase)
   '("war" "peace" "peace" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.1949 (:tag :sacla)
 (assert-true
  (equal
   (substitute "peace"
               "WAR"
               '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
               :start 1
               :end 7
               :count 2
               :from-end t
               :test-not (complement #'string=)
               :key #'string-upcase)
   '("war" "War" "WAr" "WAR" "war" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.1950 (:tag :sacla)
 (assert-true
  (equal
   (substitute "peace"
               "WAR"
               '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
               :start 1
               :end 7
               :count 0
               :from-end t
               :test-not (complement #'string=)
               :key #'string-upcase)
   '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.1951 (:tag :sacla)
 (assert-true
  (equal
   (substitute "peace"
               "WAR"
               '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
               :start 1
               :end 7
               :count -2
               :from-end t
               :test-not (complement #'string=)
               :key #'string-upcase)
   '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.1952 (:tag :sacla)
 (assert-true
  (equal
   (substitute "peace"
               "WAR"
               '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
               :start 1
               :end 7
               :count nil
               :from-end t
               :test-not (complement #'string=)
               :key #'string-upcase)
   '("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.1953 (:tag :sacla)
 (assert-true
  (equal
   (substitute "peace"
               "WAR"
               '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
               :start 1
               :end 7
               :count 6
               :from-end t
               :test-not (complement #'string=)
               :key #'string-upcase)
   '("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.1954 (:tag :sacla)
 (assert-true
  (equal
   (substitute "peace"
               "WAR"
               '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
               :start 1
               :end 7
               :count 7
               :from-end t
               :test-not (complement #'string=)
               :key #'string-upcase)
   '("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.1955 (:tag :sacla)
 (assert-true
  (equal
   (substitute "peace"
               "WAR"
               '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
               :start 1
               :end 7
               :count 100
               :from-end t
               :test-not (complement #'string=)
               :key #'string-upcase)
   '("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.1956 (:tag :sacla)
 (assert-true (equalp (substitute 'a 'x #(x y z)) #(a y z))))
(define-test sacla-must-sequence.1957 (:tag :sacla)
 (assert-true (equalp (substitute 'b 'y #(x y z)) #(x b z))))
(define-test sacla-must-sequence.1958 (:tag :sacla)
 (assert-true (equalp (substitute 'c 'z #(x y z)) #(x y c))))
(define-test sacla-must-sequence.1959 (:tag :sacla)
 (assert-true (equalp (substitute 'a 'p #(x y z)) #(x y z))))
(define-test sacla-must-sequence.1960 (:tag :sacla)
 (assert-true (equalp (substitute 'a 'x #()) #())))
(define-test sacla-must-sequence.1961 (:tag :sacla)
 (assert-true
  (equalp (substitute #\x #\b #(#\a #\b #\c #\d #\e) :test #'char<)
          #(#\a #\b #\x #\x #\x))))
(define-test sacla-must-sequence.1962 (:tag :sacla)
 (assert-true
  (equalp
   (substitute #\x #\b #(#\a #\b #\c #\d #\e) :test-not (complement #'char<))
   #(#\a #\b #\x #\x #\x))))
(define-test sacla-must-sequence.1963 (:tag :sacla)
 (assert-true
  (equalp (substitute '(a) 'x #((x) (y) (z)) :key #'car) #((a) (y) (z)))))
(define-test sacla-must-sequence.1964 (:tag :sacla)
 (assert-true
  (equalp (substitute 'c 'b #(a b a b a b a b)) #(a c a c a c a c))))
(define-test sacla-must-sequence.1965 (:tag :sacla)
 (assert-true (equalp (substitute 'a 'b #(b b b)) #(a a a))))
(define-test sacla-must-sequence.1966 (:tag :sacla)
 (assert-true
  (equalp (substitute 'z 'x #(a x b x c x d x e x f))
          #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.1967 (:tag :sacla)
 (assert-true
  (equalp (substitute 'z 'x #(a x b x c x d x e x f) :count nil)
          #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.1968 (:tag :sacla)
 (assert-true
  (equalp (substitute 'z 'x #(a x b x c x d x e x f) :count 0)
          #(a x b x c x d x e x f))))
(define-test sacla-must-sequence.1969 (:tag :sacla)
 (assert-true
  (equalp (substitute 'z 'x #(a x b x c x d x e x f) :count -100)
          #(a x b x c x d x e x f))))
(define-test sacla-must-sequence.1970 (:tag :sacla)
 (assert-true
  (equalp (substitute 'z 'x #(a x b x c x d x e x f) :count 1)
          #(a z b x c x d x e x f))))
(define-test sacla-must-sequence.1971 (:tag :sacla)
 (assert-true
  (equalp (substitute 'z 'x #(a x b x c x d x e x f) :count 2)
          #(a z b z c x d x e x f))))
(define-test sacla-must-sequence.1972 (:tag :sacla)
 (assert-true
  (equalp (substitute 'z 'x #(a x b x c x d x e x f) :count 3)
          #(a z b z c z d x e x f))))
(define-test sacla-must-sequence.1973 (:tag :sacla)
 (assert-true
  (equalp (substitute 'z 'x #(a x b x c x d x e x f) :count 4)
          #(a z b z c z d z e x f))))
(define-test sacla-must-sequence.1974 (:tag :sacla)
 (assert-true
  (equalp (substitute 'z 'x #(a x b x c x d x e x f) :count 5)
          #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.1975 (:tag :sacla)
 (assert-true
  (equalp (substitute 'z 'x #(a x b x c x d x e x f) :count 6)
          #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.1976 (:tag :sacla)
 (assert-true
  (equalp (substitute 'z 'x #(a x b x c x d x e x f) :count 7)
          #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.1977 (:tag :sacla)
 (assert-true
  (equalp (substitute 'z 'x #(a x b x c x d x e x f) :count nil :from-end t)
          #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.1978 (:tag :sacla)
 (assert-true
  (equalp (substitute 'z 'x #(a x b x c x d x e x f) :count 0 :from-end t)
          #(a x b x c x d x e x f))))
(define-test sacla-must-sequence.1979 (:tag :sacla)
 (assert-true
  (equalp (substitute 'z 'x #(a x b x c x d x e x f) :count -100 :from-end t)
          #(a x b x c x d x e x f))))
(define-test sacla-must-sequence.1980 (:tag :sacla)
 (assert-true
  (equalp (substitute 'z 'x #(a x b x c x d x e x f) :count 1 :from-end t)
          #(a x b x c x d x e z f))))
(define-test sacla-must-sequence.1981 (:tag :sacla)
 (assert-true
  (equalp (substitute 'z 'x #(a x b x c x d x e x f) :count 2 :from-end t)
          #(a x b x c x d z e z f))))
(define-test sacla-must-sequence.1982 (:tag :sacla)
 (assert-true
  (equalp (substitute 'z 'x #(a x b x c x d x e x f) :count 3 :from-end t)
          #(a x b x c z d z e z f))))
(define-test sacla-must-sequence.1983 (:tag :sacla)
 (assert-true
  (equalp (substitute 'z 'x #(a x b x c x d x e x f) :count 4 :from-end t)
          #(a x b z c z d z e z f))))
(define-test sacla-must-sequence.1984 (:tag :sacla)
 (assert-true
  (equalp (substitute 'z 'x #(a x b x c x d x e x f) :count 5 :from-end t)
          #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.1985 (:tag :sacla)
 (assert-true
  (equalp (substitute 'z 'x #(a x b x c x d x e x f) :count 6 :from-end t)
          #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.1986 (:tag :sacla)
 (assert-true
  (equalp (substitute 'z 'x #(a x b x c x d x e x f) :count 7 :from-end t)
          #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.1987 (:tag :sacla)
 (assert-true
  (equalp (substitute 'z 'x #(a x b x c x d x e x f) :start 2 :count 1)
          #(a x b z c x d x e x f))))
(define-test sacla-must-sequence.1988 (:tag :sacla)
 (assert-true
  (equalp
   (substitute 'z 'x #(a x b x c x d x e x f) :start 2 :end nil :count 1)
   #(a x b z c x d x e x f))))
(define-test sacla-must-sequence.1989 (:tag :sacla)
 (assert-true
  (equalp
   (substitute 'z 'x #(a x b x c x d x e x f) :start 2 :end 6 :count 100)
   #(a x b z c z d x e x f))))
(define-test sacla-must-sequence.1990 (:tag :sacla)
 (assert-true
  (equalp
   (substitute 'z 'x #(a x b x c x d x e x f) :start 2 :end 11 :count 100)
   #(a x b z c z d z e z f))))
(define-test sacla-must-sequence.1991 (:tag :sacla)
 (assert-true
  (equalp (substitute 'z 'x #(a x b x c x d x e x f) :start 2 :end 8 :count 10)
          #(a x b z c z d z e x f))))
(define-test sacla-must-sequence.1992 (:tag :sacla)
 (assert-true
  (equalp
   (substitute 'z
               'x
               #(a x b x c x d x e x f)
               :start 2
               :end 8
               :count 2
               :from-end t)
   #(a x b x c z d z e x f))))
(define-test sacla-must-sequence.1993 (:tag :sacla)
 (assert-true
  (equalp (substitute #\z #\c #(#\a #\b #\c #\d #\e #\f) :test #'char<)
          #(#\a #\b #\c #\z #\z #\z))))
(define-test sacla-must-sequence.1994 (:tag :sacla)
 (assert-true
  (equalp
   (substitute #\z
               #\c
               #(#\a #\b #\c #\d #\e #\f)
               :test-not (complement #'char<))
   #(#\a #\b #\c #\z #\z #\z))))
(define-test sacla-must-sequence.1995 (:tag :sacla)
 (assert-true
  (equalp
   (substitute "peace" "war" #("love" "hate" "war" "peace") :test #'equal)
   #("love" "hate" "peace" "peace"))))
(define-test sacla-must-sequence.1996 (:tag :sacla)
 (assert-true
  (equalp
   (substitute "peace"
               "war"
               #("love" "hate" "war" "peace")
               :test-not (complement #'equal))
   #("love" "hate" "peace" "peace"))))
(define-test sacla-must-sequence.1997 (:tag :sacla)
 (assert-true
  (equalp
   (substitute "peace" "war" #("war" "War" "WAr" "WAR") :test #'string-equal)
   #("peace" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.1998 (:tag :sacla)
 (assert-true
  (equalp
   (substitute "peace"
               "war"
               #("war" "War" "WAr" "WAR")
               :test-not (complement #'string-equal))
   #("peace" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.1999 (:tag :sacla)
 (assert-true
  (equalp (substitute "peace" "WAR" #("war" "War" "WAr" "WAR") :test #'string=)
          #("war" "War" "WAr" "peace"))))
(define-test sacla-must-sequence.2000 (:tag :sacla)
 (assert-true
  (equalp
   (substitute "peace"
               "WAR"
               #("war" "War" "WAr" "WAR")
               :test-not (complement #'string=))
   #("war" "War" "WAr" "peace"))))
(define-test sacla-must-sequence.2001 (:tag :sacla)
 (assert-true
  (equalp
   (substitute "peace"
               "WAR"
               #("war" "War" "WAr" "WAR")
               :test #'string=
               :key #'string-upcase)
   #("peace" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2002 (:tag :sacla)
 (assert-true
  (equalp
   (substitute "peace"
               "WAR"
               #("war" "War" "WAr" "WAR")
               :test-not (complement #'string=)
               :key #'string-upcase)
   #("peace" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2003 (:tag :sacla)
 (assert-true
  (equalp
   (substitute "peace"
               "WAR"
               #("war" "War" "WAr" "WAR")
               :start 1
               :end 2
               :test #'string=
               :key #'string-upcase)
   #("war" "peace" "WAr" "WAR"))))
(define-test sacla-must-sequence.2004 (:tag :sacla)
 (assert-true
  (equalp
   (substitute "peace"
               "WAR"
               #("war" "War" "WAr" "WAR")
               :start 1
               :end 2
               :test-not (complement #'string=)
               :key #'string-upcase)
   #("war" "peace" "WAr" "WAR"))))
(define-test sacla-must-sequence.2005 (:tag :sacla)
 (assert-true
  (equalp
   (substitute "peace"
               "WAR"
               #("war" "War" "WAr" "WAR")
               :start 1
               :end nil
               :test #'string=
               :key #'string-upcase)
   #("war" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2006 (:tag :sacla)
 (assert-true
  (equalp
   (substitute "peace"
               "WAR"
               #("war" "War" "WAr" "WAR")
               :start 1
               :end nil
               :test-not (complement #'string=)
               :key #'string-upcase)
   #("war" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2007 (:tag :sacla)
 (assert-true
  (equalp
   (substitute "peace"
               "war"
               #("war" "War" "WAr" "WAR")
               :test #'string=
               :key #'string-upcase)
   #("war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2008 (:tag :sacla)
 (assert-true
  (equalp
   (substitute "peace"
               "war"
               #("war" "War" "WAr" "WAR")
               :test-not (complement #'string=)
               :key #'string-upcase)
   #("war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2009 (:tag :sacla)
 (assert-true
  (equalp
   (substitute "peace"
               "WAR"
               #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
               :start 1
               :end 7
               :count 1
               :test-not (complement #'string=)
               :key #'string-upcase)
   #("war" "peace" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2010 (:tag :sacla)
 (assert-true
  (equalp
   (substitute "peace"
               "WAR"
               #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
               :start 1
               :end 7
               :count 2
               :test-not (complement #'string=)
               :key #'string-upcase)
   #("war" "peace" "peace" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2011 (:tag :sacla)
 (assert-true
  (equalp
   (substitute "peace"
               "WAR"
               #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
               :start 1
               :end 7
               :count 2
               :from-end t
               :test-not (complement #'string=)
               :key #'string-upcase)
   #("war" "War" "WAr" "WAR" "war" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2012 (:tag :sacla)
 (assert-true
  (equalp
   (substitute "peace"
               "WAR"
               #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
               :start 1
               :end 7
               :count 0
               :from-end t
               :test-not (complement #'string=)
               :key #'string-upcase)
   #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2013 (:tag :sacla)
 (assert-true
  (equalp
   (substitute "peace"
               "WAR"
               #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
               :start 1
               :end 7
               :count -2
               :from-end t
               :test-not (complement #'string=)
               :key #'string-upcase)
   #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2014 (:tag :sacla)
 (assert-true
  (equalp
   (substitute "peace"
               "WAR"
               #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
               :start 1
               :end 7
               :count nil
               :from-end t
               :test-not (complement #'string=)
               :key #'string-upcase)
   #("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2015 (:tag :sacla)
 (assert-true
  (equalp
   (substitute "peace"
               "WAR"
               #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
               :start 1
               :end 7
               :count 6
               :from-end t
               :test-not (complement #'string=)
               :key #'string-upcase)
   #("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2016 (:tag :sacla)
 (assert-true
  (equalp
   (substitute "peace"
               "WAR"
               #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
               :start 1
               :end 7
               :count 7
               :from-end t
               :test-not (complement #'string=)
               :key #'string-upcase)
   #("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2017 (:tag :sacla)
 (assert-true
  (equalp
   (substitute "peace"
               "WAR"
               #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
               :start 1
               :end 7
               :count 100
               :from-end t
               :test-not (complement #'string=)
               :key #'string-upcase)
   #("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2018 (:tag :sacla)
 (assert-true (string= (substitute #\A #\a "abcabc") "AbcAbc")))
(define-test sacla-must-sequence.2019 (:tag :sacla)
 (assert-true (string= (substitute #\A #\a "") "")))
(define-test sacla-must-sequence.2020 (:tag :sacla)
 (assert-true (string= (substitute #\A #\a "xyz") "xyz")))
(define-test sacla-must-sequence.2021 (:tag :sacla)
 (assert-true
  (string= (substitute #\A #\a "aaaaaaaaaa" :start 5 :end nil) "aaaaaAAAAA")))
(define-test sacla-must-sequence.2022 (:tag :sacla)
 (assert-true
  (string= (substitute #\x #\5 "0123456789" :test #'char<) "012345xxxx")))
(define-test sacla-must-sequence.2023 (:tag :sacla)
 (assert-true
  (string= (substitute #\x #\5 "0123456789" :test #'char>) "xxxxx56789")))
(define-test sacla-must-sequence.2024 (:tag :sacla)
 (assert-true
  (string= (substitute #\x #\D "abcdefg" :key #'char-upcase :test #'char>)
           "xxxdefg")))
(define-test sacla-must-sequence.2025 (:tag :sacla)
 (assert-true
  (string=
   (substitute #\x
               #\D
               "abcdefg"
               :start 1
               :end 2
               :key #'char-upcase
               :test #'char>)
   "axcdefg")))
(define-test sacla-must-sequence.2026 (:tag :sacla)
 (assert-true
  (string= (substitute #\A #\a "aaaaaaaaaa" :count 2) "AAaaaaaaaa")))
(define-test sacla-must-sequence.2027 (:tag :sacla)
 (assert-true
  (string= (substitute #\A #\a "aaaaaaaaaa" :count -1) "aaaaaaaaaa")))
(define-test sacla-must-sequence.2028 (:tag :sacla)
 (assert-true
  (string= (substitute #\A #\a "aaaaaaaaaa" :count 0) "aaaaaaaaaa")))
(define-test sacla-must-sequence.2029 (:tag :sacla)
 (assert-true
  (string= (substitute #\A #\a "aaaaaaaaaa" :count nil) "AAAAAAAAAA")))
(define-test sacla-must-sequence.2030 (:tag :sacla)
 (assert-true
  (string= (substitute #\A #\a "aaaaaaaaaa" :count 100) "AAAAAAAAAA")))
(define-test sacla-must-sequence.2031 (:tag :sacla)
 (assert-true
  (string= (substitute #\A #\a "aaaaaaaaaa" :count 9) "AAAAAAAAAa")))
(define-test sacla-must-sequence.2032 (:tag :sacla)
 (assert-true
  (string= (substitute #\A #\a "aaaaaaaaaa" :count 9 :from-end t)
           "aAAAAAAAAA")))
(define-test sacla-must-sequence.2033 (:tag :sacla)
 (assert-true
  (string= (substitute #\A #\a "aaaaaaaaaa" :start 2 :end 8 :count 3)
           "aaAAAaaaaa")))
(define-test sacla-must-sequence.2034 (:tag :sacla)
 (assert-true
  (string=
   (substitute #\A #\a "aaaaaaaaaa" :start 2 :end 8 :from-end t :count 3)
   "aaaaaAAAaa")))
(define-test sacla-must-sequence.2035 (:tag :sacla)
 (assert-true
  (string=
   (substitute #\x #\A "aaaaaaaaaa" :start 2 :end 8 :from-end t :count 3)
   "aaaaaaaaaa")))
(define-test sacla-must-sequence.2036 (:tag :sacla)
 (assert-true
  (string=
   (substitute #\X
               #\A
               "aaaaaaaaaa"
               :start 2
               :end 8
               :from-end t
               :key #'char-upcase
               :count 3)
   "aaaaaXXXaa")))
(define-test sacla-must-sequence.2037 (:tag :sacla)
 (assert-true
  (string=
   (substitute #\X
               #\D
               "abcdefghij"
               :start 2
               :end 8
               :from-end t
               :key #'char-upcase
               :test #'char<
               :count 3)
   "abcdeXXXij")))
(define-test sacla-must-sequence.2038 (:tag :sacla)
 (assert-true (equalp (substitute 0 1 #*1111) #*0000)))
(define-test sacla-must-sequence.2039 (:tag :sacla)
 (assert-true (equalp (substitute 0 1 #*1111 :start 1 :end nil) #*1000)))
(define-test sacla-must-sequence.2040 (:tag :sacla)
 (assert-true (equalp (substitute 0 1 #*1111 :start 1 :end 3) #*1001)))
(define-test sacla-must-sequence.2041 (:tag :sacla)
 (assert-true (equalp (substitute 0 1 #*11111111 :start 1 :end 7) #*10000001)))
(define-test sacla-must-sequence.2042 (:tag :sacla)
 (assert-true
  (equalp (substitute 0 1 #*11111111 :start 1 :end 7 :count 3) #*10001111)))
(define-test sacla-must-sequence.2043 (:tag :sacla)
 (assert-true
  (equalp (substitute 0 1 #*11111111 :start 1 :end 7 :count 3 :from-end t)
          #*11110001)))
(define-test sacla-must-sequence.2044 (:tag :sacla)
 (assert-true
  (equalp
   (substitute 1
               1
               #*10101010
               :start 1
               :end 7
               :count 3
               :from-end t
               :key #'(lambda (x) (if (zerop x) 1 0)))
   #*11111110)))
(define-test sacla-must-sequence.2045 (:tag :sacla)
 (assert-true
  (equalp
   (substitute 1
               1
               #*10101010
               :start 1
               :end 7
               :count 3
               :from-end t
               :key #'(lambda (x) (if (zerop x) 1 0))
               :test #'>=)
   #*10101110)))
(define-test sacla-must-sequence.2046 (:tag :sacla)
 (assert-true
  (equal (substitute-if 'a #'(lambda (arg) (eq arg 'x)) '(x y z)) '(a y z))))
(define-test sacla-must-sequence.2047 (:tag :sacla)
 (assert-true
  (equal (substitute-if 'b #'(lambda (arg) (eq arg 'y)) '(x y z)) '(x b z))))
(define-test sacla-must-sequence.2048 (:tag :sacla)
 (assert-true
  (equal (substitute-if 'c #'(lambda (arg) (eq arg 'z)) '(x y z)) '(x y c))))
(define-test sacla-must-sequence.2049 (:tag :sacla)
 (assert-true
  (equal (substitute-if 'a #'(lambda (arg) (eq arg 'p)) '(x y z)) '(x y z))))
(define-test sacla-must-sequence.2050 (:tag :sacla)
 (assert-true
  (equal (substitute-if 'a #'(lambda (arg) (eq arg 'x)) 'nil) 'nil)))
(define-test sacla-must-sequence.2051 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if #\x #'(lambda (arg) (char< #\b arg)) '(#\a #\b #\c #\d #\e))
   '(#\a #\b #\x #\x #\x))))
(define-test sacla-must-sequence.2052 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if '(a) #'(lambda (arg) (eq arg 'x)) '((x) (y) (z)) :key #'car)
   '((a) (y) (z)))))
(define-test sacla-must-sequence.2053 (:tag :sacla)
 (assert-true
  (equal (substitute-if 'c #'(lambda (arg) (eq arg 'b)) '(a b a b a b a b))
         '(a c a c a c a c))))
(define-test sacla-must-sequence.2054 (:tag :sacla)
 (assert-true
  (equal (substitute-if 'a #'(lambda (arg) (eq arg 'b)) '(b b b)) '(a a a))))
(define-test sacla-must-sequence.2055 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if 'z #'(lambda (arg) (eq arg 'x)) '(a x b x c x d x e x f))
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2056 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  '(a x b x c x d x e x f)
                  :count nil)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2057 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  '(a x b x c x d x e x f)
                  :count 0)
   '(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2058 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  '(a x b x c x d x e x f)
                  :count -100)
   '(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2059 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  '(a x b x c x d x e x f)
                  :count 1)
   '(a z b x c x d x e x f))))
(define-test sacla-must-sequence.2060 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  '(a x b x c x d x e x f)
                  :count 2)
   '(a z b z c x d x e x f))))
(define-test sacla-must-sequence.2061 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  '(a x b x c x d x e x f)
                  :count 3)
   '(a z b z c z d x e x f))))
(define-test sacla-must-sequence.2062 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  '(a x b x c x d x e x f)
                  :count 4)
   '(a z b z c z d z e x f))))
(define-test sacla-must-sequence.2063 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  '(a x b x c x d x e x f)
                  :count 5)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2064 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  '(a x b x c x d x e x f)
                  :count 6)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2065 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  '(a x b x c x d x e x f)
                  :count 7)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2066 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  '(a x b x c x d x e x f)
                  :count nil
                  :from-end t)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2067 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  '(a x b x c x d x e x f)
                  :count 0
                  :from-end t)
   '(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2068 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  '(a x b x c x d x e x f)
                  :count -100
                  :from-end t)
   '(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2069 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  '(a x b x c x d x e x f)
                  :count 1
                  :from-end t)
   '(a x b x c x d x e z f))))
(define-test sacla-must-sequence.2070 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  '(a x b x c x d x e x f)
                  :count 2
                  :from-end t)
   '(a x b x c x d z e z f))))
(define-test sacla-must-sequence.2071 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  '(a x b x c x d x e x f)
                  :count 3
                  :from-end t)
   '(a x b x c z d z e z f))))
(define-test sacla-must-sequence.2072 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  '(a x b x c x d x e x f)
                  :count 4
                  :from-end t)
   '(a x b z c z d z e z f))))
(define-test sacla-must-sequence.2073 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  '(a x b x c x d x e x f)
                  :count 5
                  :from-end t)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2074 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  '(a x b x c x d x e x f)
                  :count 6
                  :from-end t)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2075 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  '(a x b x c x d x e x f)
                  :count 7
                  :from-end t)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2076 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  '(a x b x c x d x e x f)
                  :start 2
                  :count 1)
   '(a x b z c x d x e x f))))
(define-test sacla-must-sequence.2077 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  '(a x b x c x d x e x f)
                  :start 2
                  :end nil
                  :count 1)
   '(a x b z c x d x e x f))))
(define-test sacla-must-sequence.2078 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  '(a x b x c x d x e x f)
                  :start 2
                  :end 6
                  :count 100)
   '(a x b z c z d x e x f))))
(define-test sacla-must-sequence.2079 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  '(a x b x c x d x e x f)
                  :start 2
                  :end 11
                  :count 100)
   '(a x b z c z d z e z f))))
(define-test sacla-must-sequence.2080 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  '(a x b x c x d x e x f)
                  :start 2
                  :end 8
                  :count 10)
   '(a x b z c z d z e x f))))
(define-test sacla-must-sequence.2081 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  '(a x b x c x d x e x f)
                  :start 2
                  :end 8
                  :count 2
                  :from-end t)
   '(a x b x c z d z e x f))))
(define-test sacla-must-sequence.2082 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if #\z
                  #'(lambda (arg) (char< #\c arg))
                  '(#\a #\b #\c #\d #\e #\f))
   '(#\a #\b #\c #\z #\z #\z))))
(define-test sacla-must-sequence.2083 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if "peace"
                  #'(lambda (arg) (equal "war" arg))
                  '("love" "hate" "war" "peace"))
   '("love" "hate" "peace" "peace"))))
(define-test sacla-must-sequence.2084 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if "peace"
                  #'(lambda (arg) (string-equal "war" arg))
                  '("war" "War" "WAr" "WAR"))
   '("peace" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2085 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if "peace"
                  #'(lambda (arg) (string= "WAR" arg))
                  '("war" "War" "WAr" "WAR")
                  :key #'string-upcase)
   '("peace" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2086 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if "peace"
                  #'(lambda (arg) (string= "WAR" arg))
                  '("war" "War" "WAr" "WAR")
                  :start 1
                  :end 2
                  :key #'string-upcase)
   '("war" "peace" "WAr" "WAR"))))
(define-test sacla-must-sequence.2087 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if "peace"
                  #'(lambda (arg) (string= "WAR" arg))
                  '("war" "War" "WAr" "WAR")
                  :start 1
                  :end nil
                  :key #'string-upcase)
   '("war" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2088 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if "peace"
                  #'(lambda (arg) (string= "war" arg))
                  '("war" "War" "WAr" "WAR")
                  :key #'string-upcase)
   '("war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2089 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if "peace"
                  #'(lambda (arg) (string= "WAR" arg))
                  '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                  :start 1
                  :end 7
                  :count 1
                  :key #'string-upcase)
   '("war" "peace" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2090 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if "peace"
                  #'(lambda (arg) (string= "WAR" arg))
                  '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                  :start 1
                  :end 7
                  :count 2
                  :key #'string-upcase)
   '("war" "peace" "peace" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2091 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if "peace"
                  #'(lambda (arg) (string= "WAR" arg))
                  '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                  :start 1
                  :end 7
                  :count 2
                  :from-end t
                  :key #'string-upcase)
   '("war" "War" "WAr" "WAR" "war" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2092 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if "peace"
                  #'(lambda (arg) (string= "WAR" arg))
                  '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                  :start 1
                  :end 7
                  :count 0
                  :from-end t
                  :key #'string-upcase)
   '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2093 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if "peace"
                  #'(lambda (arg) (string= "WAR" arg))
                  '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                  :start 1
                  :end 7
                  :count -2
                  :from-end t
                  :key #'string-upcase)
   '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2094 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if "peace"
                  #'(lambda (arg) (string= "WAR" arg))
                  '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                  :start 1
                  :end 7
                  :count nil
                  :from-end t
                  :key #'string-upcase)
   '("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2095 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if "peace"
                  #'(lambda (arg) (string= "WAR" arg))
                  '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                  :start 1
                  :end 7
                  :count 6
                  :from-end t
                  :key #'string-upcase)
   '("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2096 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if "peace"
                  #'(lambda (arg) (string= "WAR" arg))
                  '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                  :start 1
                  :end 7
                  :count 7
                  :from-end t
                  :key #'string-upcase)
   '("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2097 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if "peace"
                  #'(lambda (arg) (string= "WAR" arg))
                  '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                  :start 1
                  :end 7
                  :count 100
                  :from-end t
                  :key #'string-upcase)
   '("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2098 (:tag :sacla)
 (assert-true
  (equalp (substitute-if 'a #'(lambda (arg) (eq arg 'x)) #(x y z)) #(a y z))))
(define-test sacla-must-sequence.2099 (:tag :sacla)
 (assert-true
  (equalp (substitute-if 'b #'(lambda (arg) (eq arg 'y)) #(x y z)) #(x b z))))
(define-test sacla-must-sequence.2100 (:tag :sacla)
 (assert-true
  (equalp (substitute-if 'c #'(lambda (arg) (eq arg 'z)) #(x y z)) #(x y c))))
(define-test sacla-must-sequence.2101 (:tag :sacla)
 (assert-true
  (equalp (substitute-if 'a #'(lambda (arg) (eq arg 'p)) #(x y z)) #(x y z))))
(define-test sacla-must-sequence.2102 (:tag :sacla)
 (assert-true (equalp (substitute-if 'a #'(lambda (arg) (eq arg 'x)) #()) #())))
(define-test sacla-must-sequence.2103 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if #\x #'(lambda (arg) (char< #\b arg)) #(#\a #\b #\c #\d #\e))
   #(#\a #\b #\x #\x #\x))))
(define-test sacla-must-sequence.2104 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if '(a) #'(lambda (arg) (eq arg 'x)) #((x) (y) (z)) :key #'car)
   #((a) (y) (z)))))
(define-test sacla-must-sequence.2105 (:tag :sacla)
 (assert-true
  (equalp (substitute-if 'c #'(lambda (arg) (eq arg 'b)) #(a b a b a b a b))
          #(a c a c a c a c))))
(define-test sacla-must-sequence.2106 (:tag :sacla)
 (assert-true
  (equalp (substitute-if 'a #'(lambda (arg) (eq arg 'b)) #(b b b)) #(a a a))))
(define-test sacla-must-sequence.2107 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if 'z #'(lambda (arg) (eq arg 'x)) #(a x b x c x d x e x f))
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2108 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  #(a x b x c x d x e x f)
                  :count nil)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2109 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  #(a x b x c x d x e x f)
                  :count 0)
   #(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2110 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  #(a x b x c x d x e x f)
                  :count -100)
   #(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2111 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  #(a x b x c x d x e x f)
                  :count 1)
   #(a z b x c x d x e x f))))
(define-test sacla-must-sequence.2112 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  #(a x b x c x d x e x f)
                  :count 2)
   #(a z b z c x d x e x f))))
(define-test sacla-must-sequence.2113 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  #(a x b x c x d x e x f)
                  :count 3)
   #(a z b z c z d x e x f))))
(define-test sacla-must-sequence.2114 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  #(a x b x c x d x e x f)
                  :count 4)
   #(a z b z c z d z e x f))))
(define-test sacla-must-sequence.2115 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  #(a x b x c x d x e x f)
                  :count 5)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2116 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  #(a x b x c x d x e x f)
                  :count 6)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2117 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  #(a x b x c x d x e x f)
                  :count 7)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2118 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  #(a x b x c x d x e x f)
                  :count nil
                  :from-end t)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2119 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  #(a x b x c x d x e x f)
                  :count 0
                  :from-end t)
   #(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2120 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  #(a x b x c x d x e x f)
                  :count -100
                  :from-end t)
   #(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2121 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  #(a x b x c x d x e x f)
                  :count 1
                  :from-end t)
   #(a x b x c x d x e z f))))
(define-test sacla-must-sequence.2122 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  #(a x b x c x d x e x f)
                  :count 2
                  :from-end t)
   #(a x b x c x d z e z f))))
(define-test sacla-must-sequence.2123 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  #(a x b x c x d x e x f)
                  :count 3
                  :from-end t)
   #(a x b x c z d z e z f))))
(define-test sacla-must-sequence.2124 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  #(a x b x c x d x e x f)
                  :count 4
                  :from-end t)
   #(a x b z c z d z e z f))))
(define-test sacla-must-sequence.2125 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  #(a x b x c x d x e x f)
                  :count 5
                  :from-end t)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2126 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  #(a x b x c x d x e x f)
                  :count 6
                  :from-end t)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2127 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  #(a x b x c x d x e x f)
                  :count 7
                  :from-end t)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2128 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  #(a x b x c x d x e x f)
                  :start 2
                  :count 1)
   #(a x b z c x d x e x f))))
(define-test sacla-must-sequence.2129 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  #(a x b x c x d x e x f)
                  :start 2
                  :end nil
                  :count 1)
   #(a x b z c x d x e x f))))
(define-test sacla-must-sequence.2130 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  #(a x b x c x d x e x f)
                  :start 2
                  :end 6
                  :count 100)
   #(a x b z c z d x e x f))))
(define-test sacla-must-sequence.2131 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  #(a x b x c x d x e x f)
                  :start 2
                  :end 11
                  :count 100)
   #(a x b z c z d z e z f))))
(define-test sacla-must-sequence.2132 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  #(a x b x c x d x e x f)
                  :start 2
                  :end 8
                  :count 10)
   #(a x b z c z d z e x f))))
(define-test sacla-must-sequence.2133 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if 'z
                  #'(lambda (arg) (eq arg 'x))
                  #(a x b x c x d x e x f)
                  :start 2
                  :end 8
                  :count 2
                  :from-end t)
   #(a x b x c z d z e x f))))
(define-test sacla-must-sequence.2134 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if #\z
                  #'(lambda (arg) (char< #\c arg))
                  #(#\a #\b #\c #\d #\e #\f))
   #(#\a #\b #\c #\z #\z #\z))))
(define-test sacla-must-sequence.2135 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if "peace"
                  #'(lambda (arg) (equal "war" arg))
                  #("love" "hate" "war" "peace"))
   #("love" "hate" "peace" "peace"))))
(define-test sacla-must-sequence.2136 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if "peace"
                  #'(lambda (arg) (string-equal "war" arg))
                  #("war" "War" "WAr" "WAR"))
   #("peace" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2137 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if "peace"
                  #'(lambda (arg) (string= "WAR" arg))
                  #("war" "War" "WAr" "WAR")
                  :key #'string-upcase)
   #("peace" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2138 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if "peace"
                  #'(lambda (arg) (string= "WAR" arg))
                  #("war" "War" "WAr" "WAR")
                  :start 1
                  :end 2
                  :key #'string-upcase)
   #("war" "peace" "WAr" "WAR"))))
(define-test sacla-must-sequence.2139 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if "peace"
                  #'(lambda (arg) (string= "WAR" arg))
                  #("war" "War" "WAr" "WAR")
                  :start 1
                  :end nil
                  :key #'string-upcase)
   #("war" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2140 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if "peace"
                  #'(lambda (arg) (string= "war" arg))
                  #("war" "War" "WAr" "WAR")
                  :key #'string-upcase)
   #("war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2141 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if "peace"
                  #'(lambda (arg) (string= "WAR" arg))
                  #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                  :start 1
                  :end 7
                  :count 1
                  :key #'string-upcase)
   #("war" "peace" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2142 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if "peace"
                  #'(lambda (arg) (string= "WAR" arg))
                  #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                  :start 1
                  :end 7
                  :count 2
                  :key #'string-upcase)
   #("war" "peace" "peace" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2143 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if "peace"
                  #'(lambda (arg) (string= "WAR" arg))
                  #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                  :start 1
                  :end 7
                  :count 2
                  :from-end t
                  :key #'string-upcase)
   #("war" "War" "WAr" "WAR" "war" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2144 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if "peace"
                  #'(lambda (arg) (string= "WAR" arg))
                  #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                  :start 1
                  :end 7
                  :count 0
                  :from-end t
                  :key #'string-upcase)
   #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2145 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if "peace"
                  #'(lambda (arg) (string= "WAR" arg))
                  #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                  :start 1
                  :end 7
                  :count -2
                  :from-end t
                  :key #'string-upcase)
   #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2146 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if "peace"
                  #'(lambda (arg) (string= "WAR" arg))
                  #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                  :start 1
                  :end 7
                  :count nil
                  :from-end t
                  :key #'string-upcase)
   #("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2147 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if "peace"
                  #'(lambda (arg) (string= "WAR" arg))
                  #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                  :start 1
                  :end 7
                  :count 6
                  :from-end t
                  :key #'string-upcase)
   #("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2148 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if "peace"
                  #'(lambda (arg) (string= "WAR" arg))
                  #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                  :start 1
                  :end 7
                  :count 7
                  :from-end t
                  :key #'string-upcase)
   #("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2149 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if "peace"
                  #'(lambda (arg) (string= "WAR" arg))
                  #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                  :start 1
                  :end 7
                  :count 100
                  :from-end t
                  :key #'string-upcase)
   #("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2150 (:tag :sacla)
 (assert-true
  (string= (substitute-if #\A #'(lambda (arg) (eql #\a arg)) "abcabc")
           "AbcAbc")))
(define-test sacla-must-sequence.2151 (:tag :sacla)
 (assert-true
  (string= (substitute-if #\A #'(lambda (arg) (eql #\a arg)) "") "")))
(define-test sacla-must-sequence.2152 (:tag :sacla)
 (assert-true
  (string= (substitute-if #\A #'(lambda (arg) (eql #\a arg)) "xyz") "xyz")))
(define-test sacla-must-sequence.2153 (:tag :sacla)
 (assert-true
  (string=
   (substitute-if #\A
                  #'(lambda (arg) (eql #\a arg))
                  "aaaaaaaaaa"
                  :start 5
                  :end nil)
   "aaaaaAAAAA")))
(define-test sacla-must-sequence.2154 (:tag :sacla)
 (assert-true
  (string= (substitute-if #\x #'(lambda (arg) (char< #\5 arg)) "0123456789")
           "012345xxxx")))
(define-test sacla-must-sequence.2155 (:tag :sacla)
 (assert-true
  (string= (substitute-if #\x #'(lambda (arg) (char> #\5 arg)) "0123456789")
           "xxxxx56789")))
(define-test sacla-must-sequence.2156 (:tag :sacla)
 (assert-true
  (string=
   (substitute-if #\x
                  #'(lambda (arg) (char> #\D arg))
                  "abcdefg"
                  :key #'char-upcase)
   "xxxdefg")))
(define-test sacla-must-sequence.2157 (:tag :sacla)
 (assert-true
  (string=
   (substitute-if #\x
                  #'(lambda (arg) (char> #\D arg))
                  "abcdefg"
                  :start 1
                  :end 2
                  :key #'char-upcase)
   "axcdefg")))
(define-test sacla-must-sequence.2158 (:tag :sacla)
 (assert-true
  (string=
   (substitute-if #\A #'(lambda (arg) (eql #\a arg)) "aaaaaaaaaa" :count 2)
   "AAaaaaaaaa")))
(define-test sacla-must-sequence.2159 (:tag :sacla)
 (assert-true
  (string=
   (substitute-if #\A #'(lambda (arg) (eql #\a arg)) "aaaaaaaaaa" :count -1)
   "aaaaaaaaaa")))
(define-test sacla-must-sequence.2160 (:tag :sacla)
 (assert-true
  (string=
   (substitute-if #\A #'(lambda (arg) (eql #\a arg)) "aaaaaaaaaa" :count 0)
   "aaaaaaaaaa")))
(define-test sacla-must-sequence.2161 (:tag :sacla)
 (assert-true
  (string=
   (substitute-if #\A #'(lambda (arg) (eql #\a arg)) "aaaaaaaaaa" :count nil)
   "AAAAAAAAAA")))
(define-test sacla-must-sequence.2162 (:tag :sacla)
 (assert-true
  (string=
   (substitute-if #\A #'(lambda (arg) (eql #\a arg)) "aaaaaaaaaa" :count 100)
   "AAAAAAAAAA")))
(define-test sacla-must-sequence.2163 (:tag :sacla)
 (assert-true
  (string=
   (substitute-if #\A #'(lambda (arg) (eql #\a arg)) "aaaaaaaaaa" :count 9)
   "AAAAAAAAAa")))
(define-test sacla-must-sequence.2164 (:tag :sacla)
 (assert-true
  (string=
   (substitute-if #\A
                  #'(lambda (arg) (eql #\a arg))
                  "aaaaaaaaaa"
                  :count 9
                  :from-end t)
   "aAAAAAAAAA")))
(define-test sacla-must-sequence.2165 (:tag :sacla)
 (assert-true
  (string=
   (substitute-if #\A
                  #'(lambda (arg) (eql #\a arg))
                  "aaaaaaaaaa"
                  :start 2
                  :end 8
                  :count 3)
   "aaAAAaaaaa")))
(define-test sacla-must-sequence.2166 (:tag :sacla)
 (assert-true
  (string=
   (substitute-if #\A
                  #'(lambda (arg) (eql #\a arg))
                  "aaaaaaaaaa"
                  :start 2
                  :end 8
                  :from-end t
                  :count 3)
   "aaaaaAAAaa")))
(define-test sacla-must-sequence.2167 (:tag :sacla)
 (assert-true
  (string=
   (substitute-if #\x
                  #'(lambda (arg) (eql #\A arg))
                  "aaaaaaaaaa"
                  :start 2
                  :end 8
                  :from-end t
                  :count 3)
   "aaaaaaaaaa")))
(define-test sacla-must-sequence.2168 (:tag :sacla)
 (assert-true
  (string=
   (substitute-if #\X
                  #'(lambda (arg) (eql #\A arg))
                  "aaaaaaaaaa"
                  :start 2
                  :end 8
                  :from-end t
                  :key #'char-upcase
                  :count 3)
   "aaaaaXXXaa")))
(define-test sacla-must-sequence.2169 (:tag :sacla)
 (assert-true
  (string=
   (substitute-if #\X
                  #'(lambda (arg) (char< #\D arg))
                  "abcdefghij"
                  :start 2
                  :end 8
                  :from-end t
                  :key #'char-upcase
                  :count 3)
   "abcdeXXXij")))
(define-test sacla-must-sequence.2170 (:tag :sacla)
 (assert-true
  (equalp (substitute-if 0 #'(lambda (arg) (= 1 arg)) #*1111) #*0000)))
(define-test sacla-must-sequence.2171 (:tag :sacla)
 (assert-true
  (equalp (substitute-if 0 #'(lambda (arg) (= 1 arg)) #*1111 :start 1 :end nil)
          #*1000)))
(define-test sacla-must-sequence.2172 (:tag :sacla)
 (assert-true
  (equalp (substitute-if 0 #'(lambda (arg) (= 1 arg)) #*1111 :start 1 :end 3)
          #*1001)))
(define-test sacla-must-sequence.2173 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if 0 #'(lambda (arg) (= 1 arg)) #*11111111 :start 1 :end 7)
   #*10000001)))
(define-test sacla-must-sequence.2174 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if 0
                  #'(lambda (arg) (= 1 arg))
                  #*11111111
                  :start 1
                  :end 7
                  :count 3)
   #*10001111)))
(define-test sacla-must-sequence.2175 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if 0
                  #'(lambda (arg) (= 1 arg))
                  #*11111111
                  :start 1
                  :end 7
                  :count 3
                  :from-end t)
   #*11110001)))
(define-test sacla-must-sequence.2176 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if 1
                  #'(lambda (arg) (= 1 arg))
                  #*10101010
                  :start 1
                  :end 7
                  :count 3
                  :from-end t
                  :key #'(lambda (x) (if (zerop x) 1 0)))
   #*11111110)))
(define-test sacla-must-sequence.2177 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if 1
                  #'(lambda (arg) (>= 1 arg))
                  #*10101010
                  :start 1
                  :end 7
                  :count 3
                  :from-end t
                  :key #'(lambda (x) (if (zerop x) 1 0)))
   #*10101110)))
(define-test sacla-must-sequence.2178 (:tag :sacla)
 (assert-true
  (equal (substitute-if-not 'a #'(lambda (arg) (not (eq arg 'x))) '(x y z))
         '(a y z))))
(define-test sacla-must-sequence.2179 (:tag :sacla)
 (assert-true
  (equal (substitute-if-not 'b #'(lambda (arg) (not (eq arg 'y))) '(x y z))
         '(x b z))))
(define-test sacla-must-sequence.2180 (:tag :sacla)
 (assert-true
  (equal (substitute-if-not 'c #'(lambda (arg) (not (eq arg 'z))) '(x y z))
         '(x y c))))
(define-test sacla-must-sequence.2181 (:tag :sacla)
 (assert-true
  (equal (substitute-if-not 'a #'(lambda (arg) (not (eq arg 'p))) '(x y z))
         '(x y z))))
(define-test sacla-must-sequence.2182 (:tag :sacla)
 (assert-true
  (equal (substitute-if-not 'a #'(lambda (arg) (not (eq arg 'x))) 'nil) 'nil)))
(define-test sacla-must-sequence.2183 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not #\x
                      #'(lambda (arg) (not (char< #\b arg)))
                      '(#\a #\b #\c #\d #\e))
   '(#\a #\b #\x #\x #\x))))
(define-test sacla-must-sequence.2184 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not '(a)
                      #'(lambda (arg) (not (eq arg 'x)))
                      '((x) (y) (z))
                      :key #'car)
   '((a) (y) (z)))))
(define-test sacla-must-sequence.2185 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not 'c #'(lambda (arg) (not (eq arg 'b))) '(a b a b a b a b))
   '(a c a c a c a c))))
(define-test sacla-must-sequence.2186 (:tag :sacla)
 (assert-true
  (equal (substitute-if-not 'a #'(lambda (arg) (not (eq arg 'b))) '(b b b))
         '(a a a))))
(define-test sacla-must-sequence.2187 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      '(a x b x c x d x e x f))
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2188 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      '(a x b x c x d x e x f)
                      :count nil)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2189 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      '(a x b x c x d x e x f)
                      :count 0)
   '(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2190 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      '(a x b x c x d x e x f)
                      :count -100)
   '(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2191 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      '(a x b x c x d x e x f)
                      :count 1)
   '(a z b x c x d x e x f))))
(define-test sacla-must-sequence.2192 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      '(a x b x c x d x e x f)
                      :count 2)
   '(a z b z c x d x e x f))))
(define-test sacla-must-sequence.2193 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      '(a x b x c x d x e x f)
                      :count 3)
   '(a z b z c z d x e x f))))
(define-test sacla-must-sequence.2194 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      '(a x b x c x d x e x f)
                      :count 4)
   '(a z b z c z d z e x f))))
(define-test sacla-must-sequence.2195 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      '(a x b x c x d x e x f)
                      :count 5)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2196 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      '(a x b x c x d x e x f)
                      :count 6)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2197 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      '(a x b x c x d x e x f)
                      :count 7)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2198 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      '(a x b x c x d x e x f)
                      :count nil
                      :from-end t)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2199 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      '(a x b x c x d x e x f)
                      :count 0
                      :from-end t)
   '(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2200 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      '(a x b x c x d x e x f)
                      :count -100
                      :from-end t)
   '(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2201 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      '(a x b x c x d x e x f)
                      :count 1
                      :from-end t)
   '(a x b x c x d x e z f))))
(define-test sacla-must-sequence.2202 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      '(a x b x c x d x e x f)
                      :count 2
                      :from-end t)
   '(a x b x c x d z e z f))))
(define-test sacla-must-sequence.2203 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      '(a x b x c x d x e x f)
                      :count 3
                      :from-end t)
   '(a x b x c z d z e z f))))
(define-test sacla-must-sequence.2204 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      '(a x b x c x d x e x f)
                      :count 4
                      :from-end t)
   '(a x b z c z d z e z f))))
(define-test sacla-must-sequence.2205 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      '(a x b x c x d x e x f)
                      :count 5
                      :from-end t)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2206 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      '(a x b x c x d x e x f)
                      :count 6
                      :from-end t)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2207 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      '(a x b x c x d x e x f)
                      :count 7
                      :from-end t)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2208 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      '(a x b x c x d x e x f)
                      :start 2
                      :count 1)
   '(a x b z c x d x e x f))))
(define-test sacla-must-sequence.2209 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      '(a x b x c x d x e x f)
                      :start 2
                      :end nil
                      :count 1)
   '(a x b z c x d x e x f))))
(define-test sacla-must-sequence.2210 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      '(a x b x c x d x e x f)
                      :start 2
                      :end 6
                      :count 100)
   '(a x b z c z d x e x f))))
(define-test sacla-must-sequence.2211 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      '(a x b x c x d x e x f)
                      :start 2
                      :end 11
                      :count 100)
   '(a x b z c z d z e z f))))
(define-test sacla-must-sequence.2212 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      '(a x b x c x d x e x f)
                      :start 2
                      :end 8
                      :count 10)
   '(a x b z c z d z e x f))))
(define-test sacla-must-sequence.2213 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      '(a x b x c x d x e x f)
                      :start 2
                      :end 8
                      :count 2
                      :from-end t)
   '(a x b x c z d z e x f))))
(define-test sacla-must-sequence.2214 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not #\z
                      #'(lambda (arg) (not (char< #\c arg)))
                      '(#\a #\b #\c #\d #\e #\f))
   '(#\a #\b #\c #\z #\z #\z))))
(define-test sacla-must-sequence.2215 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not "peace"
                      #'(lambda (arg) (not (equal "war" arg)))
                      '("love" "hate" "war" "peace"))
   '("love" "hate" "peace" "peace"))))
(define-test sacla-must-sequence.2216 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not "peace"
                      #'(lambda (arg) (not (string-equal "war" arg)))
                      '("war" "War" "WAr" "WAR"))
   '("peace" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2217 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not "peace"
                      #'(lambda (arg) (not (string= "WAR" arg)))
                      '("war" "War" "WAr" "WAR")
                      :key #'string-upcase)
   '("peace" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2218 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not "peace"
                      #'(lambda (arg) (not (string= "WAR" arg)))
                      '("war" "War" "WAr" "WAR")
                      :start 1
                      :end 2
                      :key #'string-upcase)
   '("war" "peace" "WAr" "WAR"))))
(define-test sacla-must-sequence.2219 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not "peace"
                      #'(lambda (arg) (not (string= "WAR" arg)))
                      '("war" "War" "WAr" "WAR")
                      :start 1
                      :end nil
                      :key #'string-upcase)
   '("war" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2220 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not "peace"
                      #'(lambda (arg) (not (string= "war" arg)))
                      '("war" "War" "WAr" "WAR")
                      :key #'string-upcase)
   '("war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2221 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not "peace"
                      #'(lambda (arg) (not (string= "WAR" arg)))
                      '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                      :start 1
                      :end 7
                      :count 1
                      :key #'string-upcase)
   '("war" "peace" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2222 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not "peace"
                      #'(lambda (arg) (not (string= "WAR" arg)))
                      '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                      :start 1
                      :end 7
                      :count 2
                      :key #'string-upcase)
   '("war" "peace" "peace" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2223 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not "peace"
                      #'(lambda (arg) (not (string= "WAR" arg)))
                      '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                      :start 1
                      :end 7
                      :count 2
                      :from-end t
                      :key #'string-upcase)
   '("war" "War" "WAr" "WAR" "war" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2224 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not "peace"
                      #'(lambda (arg) (not (string= "WAR" arg)))
                      '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                      :start 1
                      :end 7
                      :count 0
                      :from-end t
                      :key #'string-upcase)
   '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2225 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not "peace"
                      #'(lambda (arg) (not (string= "WAR" arg)))
                      '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                      :start 1
                      :end 7
                      :count -2
                      :from-end t
                      :key #'string-upcase)
   '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2226 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not "peace"
                      #'(lambda (arg) (not (string= "WAR" arg)))
                      '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                      :start 1
                      :end 7
                      :count nil
                      :from-end t
                      :key #'string-upcase)
   '("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2227 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not "peace"
                      #'(lambda (arg) (not (string= "WAR" arg)))
                      '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                      :start 1
                      :end 7
                      :count 6
                      :from-end t
                      :key #'string-upcase)
   '("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2228 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not "peace"
                      #'(lambda (arg) (not (string= "WAR" arg)))
                      '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                      :start 1
                      :end 7
                      :count 7
                      :from-end t
                      :key #'string-upcase)
   '("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2229 (:tag :sacla)
 (assert-true
  (equal
   (substitute-if-not "peace"
                      #'(lambda (arg) (not (string= "WAR" arg)))
                      '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                      :start 1
                      :end 7
                      :count 100
                      :from-end t
                      :key #'string-upcase)
   '("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2230 (:tag :sacla)
 (assert-true
  (equalp (substitute-if-not 'a #'(lambda (arg) (not (eq arg 'x))) #(x y z))
          #(a y z))))
(define-test sacla-must-sequence.2231 (:tag :sacla)
 (assert-true
  (equalp (substitute-if-not 'b #'(lambda (arg) (not (eq arg 'y))) #(x y z))
          #(x b z))))
(define-test sacla-must-sequence.2232 (:tag :sacla)
 (assert-true
  (equalp (substitute-if-not 'c #'(lambda (arg) (not (eq arg 'z))) #(x y z))
          #(x y c))))
(define-test sacla-must-sequence.2233 (:tag :sacla)
 (assert-true
  (equalp (substitute-if-not 'a #'(lambda (arg) (not (eq arg 'p))) #(x y z))
          #(x y z))))
(define-test sacla-must-sequence.2234 (:tag :sacla)
 (assert-true
  (equalp (substitute-if-not 'a #'(lambda (arg) (not (eq arg 'x))) #()) #())))
(define-test sacla-must-sequence.2235 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not #\x
                      #'(lambda (arg) (not (char< #\b arg)))
                      #(#\a #\b #\c #\d #\e))
   #(#\a #\b #\x #\x #\x))))
(define-test sacla-must-sequence.2236 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not '(a)
                      #'(lambda (arg) (not (eq arg 'x)))
                      #((x) (y) (z))
                      :key #'car)
   #((a) (y) (z)))))
(define-test sacla-must-sequence.2237 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 'c #'(lambda (arg) (not (eq arg 'b))) #(a b a b a b a b))
   #(a c a c a c a c))))
(define-test sacla-must-sequence.2238 (:tag :sacla)
 (assert-true
  (equalp (substitute-if-not 'a #'(lambda (arg) (not (eq arg 'b))) #(b b b))
          #(a a a))))
(define-test sacla-must-sequence.2239 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      #(a x b x c x d x e x f))
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2240 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      #(a x b x c x d x e x f)
                      :count nil)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2241 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      #(a x b x c x d x e x f)
                      :count 0)
   #(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2242 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      #(a x b x c x d x e x f)
                      :count -100)
   #(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2243 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      #(a x b x c x d x e x f)
                      :count 1)
   #(a z b x c x d x e x f))))
(define-test sacla-must-sequence.2244 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      #(a x b x c x d x e x f)
                      :count 2)
   #(a z b z c x d x e x f))))
(define-test sacla-must-sequence.2245 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      #(a x b x c x d x e x f)
                      :count 3)
   #(a z b z c z d x e x f))))
(define-test sacla-must-sequence.2246 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      #(a x b x c x d x e x f)
                      :count 4)
   #(a z b z c z d z e x f))))
(define-test sacla-must-sequence.2247 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      #(a x b x c x d x e x f)
                      :count 5)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2248 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      #(a x b x c x d x e x f)
                      :count 6)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2249 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      #(a x b x c x d x e x f)
                      :count 7)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2250 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      #(a x b x c x d x e x f)
                      :count nil
                      :from-end t)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2251 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      #(a x b x c x d x e x f)
                      :count 0
                      :from-end t)
   #(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2252 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      #(a x b x c x d x e x f)
                      :count -100
                      :from-end t)
   #(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2253 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      #(a x b x c x d x e x f)
                      :count 1
                      :from-end t)
   #(a x b x c x d x e z f))))
(define-test sacla-must-sequence.2254 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      #(a x b x c x d x e x f)
                      :count 2
                      :from-end t)
   #(a x b x c x d z e z f))))
(define-test sacla-must-sequence.2255 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      #(a x b x c x d x e x f)
                      :count 3
                      :from-end t)
   #(a x b x c z d z e z f))))
(define-test sacla-must-sequence.2256 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      #(a x b x c x d x e x f)
                      :count 4
                      :from-end t)
   #(a x b z c z d z e z f))))
(define-test sacla-must-sequence.2257 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      #(a x b x c x d x e x f)
                      :count 5
                      :from-end t)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2258 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      #(a x b x c x d x e x f)
                      :count 6
                      :from-end t)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2259 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      #(a x b x c x d x e x f)
                      :count 7
                      :from-end t)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2260 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      #(a x b x c x d x e x f)
                      :start 2
                      :count 1)
   #(a x b z c x d x e x f))))
(define-test sacla-must-sequence.2261 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      #(a x b x c x d x e x f)
                      :start 2
                      :end nil
                      :count 1)
   #(a x b z c x d x e x f))))
(define-test sacla-must-sequence.2262 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      #(a x b x c x d x e x f)
                      :start 2
                      :end 6
                      :count 100)
   #(a x b z c z d x e x f))))
(define-test sacla-must-sequence.2263 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      #(a x b x c x d x e x f)
                      :start 2
                      :end 11
                      :count 100)
   #(a x b z c z d z e z f))))
(define-test sacla-must-sequence.2264 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      #(a x b x c x d x e x f)
                      :start 2
                      :end 8
                      :count 10)
   #(a x b z c z d z e x f))))
(define-test sacla-must-sequence.2265 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 'z
                      #'(lambda (arg) (not (eq arg 'x)))
                      #(a x b x c x d x e x f)
                      :start 2
                      :end 8
                      :count 2
                      :from-end t)
   #(a x b x c z d z e x f))))
(define-test sacla-must-sequence.2266 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not #\z
                      #'(lambda (arg) (not (char< #\c arg)))
                      #(#\a #\b #\c #\d #\e #\f))
   #(#\a #\b #\c #\z #\z #\z))))
(define-test sacla-must-sequence.2267 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not "peace"
                      #'(lambda (arg) (not (equal "war" arg)))
                      #("love" "hate" "war" "peace"))
   #("love" "hate" "peace" "peace"))))
(define-test sacla-must-sequence.2268 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not "peace"
                      #'(lambda (arg) (not (string-equal "war" arg)))
                      #("war" "War" "WAr" "WAR"))
   #("peace" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2269 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not "peace"
                      #'(lambda (arg) (not (string= "WAR" arg)))
                      #("war" "War" "WAr" "WAR")
                      :key #'string-upcase)
   #("peace" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2270 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not "peace"
                      #'(lambda (arg) (not (string= "WAR" arg)))
                      #("war" "War" "WAr" "WAR")
                      :start 1
                      :end 2
                      :key #'string-upcase)
   #("war" "peace" "WAr" "WAR"))))
(define-test sacla-must-sequence.2271 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not "peace"
                      #'(lambda (arg) (not (string= "WAR" arg)))
                      #("war" "War" "WAr" "WAR")
                      :start 1
                      :end nil
                      :key #'string-upcase)
   #("war" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2272 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not "peace"
                      #'(lambda (arg) (not (string= "war" arg)))
                      #("war" "War" "WAr" "WAR")
                      :key #'string-upcase)
   #("war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2273 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not "peace"
                      #'(lambda (arg) (not (string= "WAR" arg)))
                      #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                      :start 1
                      :end 7
                      :count 1
                      :key #'string-upcase)
   #("war" "peace" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2274 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not "peace"
                      #'(lambda (arg) (not (string= "WAR" arg)))
                      #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                      :start 1
                      :end 7
                      :count 2
                      :key #'string-upcase)
   #("war" "peace" "peace" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2275 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not "peace"
                      #'(lambda (arg) (not (string= "WAR" arg)))
                      #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                      :start 1
                      :end 7
                      :count 2
                      :from-end t
                      :key #'string-upcase)
   #("war" "War" "WAr" "WAR" "war" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2276 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not "peace"
                      #'(lambda (arg) (not (string= "WAR" arg)))
                      #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                      :start 1
                      :end 7
                      :count 0
                      :from-end t
                      :key #'string-upcase)
   #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2277 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not "peace"
                      #'(lambda (arg) (not (string= "WAR" arg)))
                      #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                      :start 1
                      :end 7
                      :count -2
                      :from-end t
                      :key #'string-upcase)
   #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2278 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not "peace"
                      #'(lambda (arg) (not (string= "WAR" arg)))
                      #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                      :start 1
                      :end 7
                      :count nil
                      :from-end t
                      :key #'string-upcase)
   #("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2279 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not "peace"
                      #'(lambda (arg) (not (string= "WAR" arg)))
                      #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                      :start 1
                      :end 7
                      :count 6
                      :from-end t
                      :key #'string-upcase)
   #("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2280 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not "peace"
                      #'(lambda (arg) (not (string= "WAR" arg)))
                      #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                      :start 1
                      :end 7
                      :count 7
                      :from-end t
                      :key #'string-upcase)
   #("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2281 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not "peace"
                      #'(lambda (arg) (not (string= "WAR" arg)))
                      #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR")
                      :start 1
                      :end 7
                      :count 100
                      :from-end t
                      :key #'string-upcase)
   #("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2282 (:tag :sacla)
 (assert-true
  (string=
   (substitute-if-not #\A #'(lambda (arg) (not (eql #\a arg))) "abcabc")
   "AbcAbc")))
(define-test sacla-must-sequence.2283 (:tag :sacla)
 (assert-true
  (string= (substitute-if-not #\A #'(lambda (arg) (not (eql #\a arg))) "") "")))
(define-test sacla-must-sequence.2284 (:tag :sacla)
 (assert-true
  (string= (substitute-if-not #\A #'(lambda (arg) (not (eql #\a arg))) "xyz")
           "xyz")))
(define-test sacla-must-sequence.2285 (:tag :sacla)
 (assert-true
  (string=
   (substitute-if-not #\A
                      #'(lambda (arg) (not (eql #\a arg)))
                      "aaaaaaaaaa"
                      :start 5
                      :end nil)
   "aaaaaAAAAA")))
(define-test sacla-must-sequence.2286 (:tag :sacla)
 (assert-true
  (string=
   (substitute-if-not #\x #'(lambda (arg) (not (char< #\5 arg))) "0123456789")
   "012345xxxx")))
(define-test sacla-must-sequence.2287 (:tag :sacla)
 (assert-true
  (string=
   (substitute-if-not #\x #'(lambda (arg) (not (char> #\5 arg))) "0123456789")
   "xxxxx56789")))
(define-test sacla-must-sequence.2288 (:tag :sacla)
 (assert-true
  (string=
   (substitute-if-not #\x
                      #'(lambda (arg) (not (char> #\D arg)))
                      "abcdefg"
                      :key #'char-upcase)
   "xxxdefg")))
(define-test sacla-must-sequence.2289 (:tag :sacla)
 (assert-true
  (string=
   (substitute-if-not #\x
                      #'(lambda (arg) (not (char> #\D arg)))
                      "abcdefg"
                      :start 1
                      :end 2
                      :key #'char-upcase)
   "axcdefg")))
(define-test sacla-must-sequence.2290 (:tag :sacla)
 (assert-true
  (string=
   (substitute-if-not #\A
                      #'(lambda (arg) (not (eql #\a arg)))
                      "aaaaaaaaaa"
                      :count 2)
   "AAaaaaaaaa")))
(define-test sacla-must-sequence.2291 (:tag :sacla)
 (assert-true
  (string=
   (substitute-if-not #\A
                      #'(lambda (arg) (not (eql #\a arg)))
                      "aaaaaaaaaa"
                      :count -1)
   "aaaaaaaaaa")))
(define-test sacla-must-sequence.2292 (:tag :sacla)
 (assert-true
  (string=
   (substitute-if-not #\A
                      #'(lambda (arg) (not (eql #\a arg)))
                      "aaaaaaaaaa"
                      :count 0)
   "aaaaaaaaaa")))
(define-test sacla-must-sequence.2293 (:tag :sacla)
 (assert-true
  (string=
   (substitute-if-not #\A
                      #'(lambda (arg) (not (eql #\a arg)))
                      "aaaaaaaaaa"
                      :count nil)
   "AAAAAAAAAA")))
(define-test sacla-must-sequence.2294 (:tag :sacla)
 (assert-true
  (string=
   (substitute-if-not #\A
                      #'(lambda (arg) (not (eql #\a arg)))
                      "aaaaaaaaaa"
                      :count 100)
   "AAAAAAAAAA")))
(define-test sacla-must-sequence.2295 (:tag :sacla)
 (assert-true
  (string=
   (substitute-if-not #\A
                      #'(lambda (arg) (not (eql #\a arg)))
                      "aaaaaaaaaa"
                      :count 9)
   "AAAAAAAAAa")))
(define-test sacla-must-sequence.2296 (:tag :sacla)
 (assert-true
  (string=
   (substitute-if-not #\A
                      #'(lambda (arg) (not (eql #\a arg)))
                      "aaaaaaaaaa"
                      :count 9
                      :from-end t)
   "aAAAAAAAAA")))
(define-test sacla-must-sequence.2297 (:tag :sacla)
 (assert-true
  (string=
   (substitute-if-not #\A
                      #'(lambda (arg) (not (eql #\a arg)))
                      "aaaaaaaaaa"
                      :start 2
                      :end 8
                      :count 3)
   "aaAAAaaaaa")))
(define-test sacla-must-sequence.2298 (:tag :sacla)
 (assert-true
  (string=
   (substitute-if-not #\A
                      #'(lambda (arg) (not (eql #\a arg)))
                      "aaaaaaaaaa"
                      :start 2
                      :end 8
                      :from-end t
                      :count 3)
   "aaaaaAAAaa")))
(define-test sacla-must-sequence.2299 (:tag :sacla)
 (assert-true
  (string=
   (substitute-if-not #\x
                      #'(lambda (arg) (not (eql #\A arg)))
                      "aaaaaaaaaa"
                      :start 2
                      :end 8
                      :from-end t
                      :count 3)
   "aaaaaaaaaa")))
(define-test sacla-must-sequence.2300 (:tag :sacla)
 (assert-true
  (string=
   (substitute-if-not #\X
                      #'(lambda (arg) (not (eql #\A arg)))
                      "aaaaaaaaaa"
                      :start 2
                      :end 8
                      :from-end t
                      :key #'char-upcase
                      :count 3)
   "aaaaaXXXaa")))
(define-test sacla-must-sequence.2301 (:tag :sacla)
 (assert-true
  (string=
   (substitute-if-not #\X
                      #'(lambda (arg) (not (char< #\D arg)))
                      "abcdefghij"
                      :start 2
                      :end 8
                      :from-end t
                      :key #'char-upcase
                      :count 3)
   "abcdeXXXij")))
(define-test sacla-must-sequence.2302 (:tag :sacla)
 (assert-true
  (equalp (substitute-if-not 0 #'(lambda (arg) (not (= 1 arg))) #*1111)
          #*0000)))
(define-test sacla-must-sequence.2303 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 0
                      #'(lambda (arg) (not (= 1 arg)))
                      #*1111
                      :start 1
                      :end nil)
   #*1000)))
(define-test sacla-must-sequence.2304 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 0
                      #'(lambda (arg) (not (= 1 arg)))
                      #*1111
                      :start 1
                      :end 3)
   #*1001)))
(define-test sacla-must-sequence.2305 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 0
                      #'(lambda (arg) (not (= 1 arg)))
                      #*11111111
                      :start 1
                      :end 7)
   #*10000001)))
(define-test sacla-must-sequence.2306 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 0
                      #'(lambda (arg) (not (= 1 arg)))
                      #*11111111
                      :start 1
                      :end 7
                      :count 3)
   #*10001111)))
(define-test sacla-must-sequence.2307 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 0
                      #'(lambda (arg) (not (= 1 arg)))
                      #*11111111
                      :start 1
                      :end 7
                      :count 3
                      :from-end t)
   #*11110001)))
(define-test sacla-must-sequence.2308 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 1
                      #'(lambda (arg) (not (= 1 arg)))
                      #*10101010
                      :start 1
                      :end 7
                      :count 3
                      :from-end t
                      :key #'(lambda (x) (if (zerop x) 1 0)))
   #*11111110)))
(define-test sacla-must-sequence.2309 (:tag :sacla)
 (assert-true
  (equalp
   (substitute-if-not 1
                      #'(lambda (arg) (not (>= 1 arg)))
                      #*10101010
                      :start 1
                      :end 7
                      :count 3
                      :from-end t
                      :key #'(lambda (x) (if (zerop x) 1 0)))
   #*10101110)))
(define-test sacla-must-sequence.2310 (:tag :sacla)
 (assert-true (equal (nsubstitute 'a 'x (copy-seq '(x y z))) '(a y z))))
(define-test sacla-must-sequence.2311 (:tag :sacla)
 (assert-true (equal (nsubstitute 'b 'y (copy-seq '(x y z))) '(x b z))))
(define-test sacla-must-sequence.2312 (:tag :sacla)
 (assert-true (equal (nsubstitute 'c 'z (copy-seq '(x y z))) '(x y c))))
(define-test sacla-must-sequence.2313 (:tag :sacla)
 (assert-true (equal (nsubstitute 'a 'p (copy-seq '(x y z))) '(x y z))))
(define-test sacla-must-sequence.2314 (:tag :sacla)
 (assert-true (equal (nsubstitute 'a 'x (copy-seq 'nil)) 'nil)))
(define-test sacla-must-sequence.2315 (:tag :sacla)
 (assert-true
  (equal (nsubstitute #\x #\b (copy-seq '(#\a #\b #\c #\d #\e)) :test #'char<)
         '(#\a #\b #\x #\x #\x))))
(define-test sacla-must-sequence.2316 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute #\x
                #\b
                (copy-seq '(#\a #\b #\c #\d #\e))
                :test-not (complement #'char<))
   '(#\a #\b #\x #\x #\x))))
(define-test sacla-must-sequence.2317 (:tag :sacla)
 (assert-true
  (equal (nsubstitute '(a) 'x (copy-seq '((x) (y) (z))) :key #'car)
         '((a) (y) (z)))))
(define-test sacla-must-sequence.2318 (:tag :sacla)
 (assert-true
  (equal (nsubstitute 'c 'b (copy-seq '(a b a b a b a b))) '(a c a c a c a c))))
(define-test sacla-must-sequence.2319 (:tag :sacla)
 (assert-true (equal (nsubstitute 'a 'b (copy-seq '(b b b))) '(a a a))))
(define-test sacla-must-sequence.2320 (:tag :sacla)
 (assert-true
  (equal (nsubstitute 'z 'x (copy-seq '(a x b x c x d x e x f)))
         '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2321 (:tag :sacla)
 (assert-true
  (equal (nsubstitute 'z 'x (copy-seq '(a x b x c x d x e x f)) :count nil)
         '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2322 (:tag :sacla)
 (assert-true
  (equal (nsubstitute 'z 'x (copy-seq '(a x b x c x d x e x f)) :count 0)
         '(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2323 (:tag :sacla)
 (assert-true
  (equal (nsubstitute 'z 'x (copy-seq '(a x b x c x d x e x f)) :count -100)
         '(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2324 (:tag :sacla)
 (assert-true
  (equal (nsubstitute 'z 'x (copy-seq '(a x b x c x d x e x f)) :count 1)
         '(a z b x c x d x e x f))))
(define-test sacla-must-sequence.2325 (:tag :sacla)
 (assert-true
  (equal (nsubstitute 'z 'x (copy-seq '(a x b x c x d x e x f)) :count 2)
         '(a z b z c x d x e x f))))
(define-test sacla-must-sequence.2326 (:tag :sacla)
 (assert-true
  (equal (nsubstitute 'z 'x (copy-seq '(a x b x c x d x e x f)) :count 3)
         '(a z b z c z d x e x f))))
(define-test sacla-must-sequence.2327 (:tag :sacla)
 (assert-true
  (equal (nsubstitute 'z 'x (copy-seq '(a x b x c x d x e x f)) :count 4)
         '(a z b z c z d z e x f))))
(define-test sacla-must-sequence.2328 (:tag :sacla)
 (assert-true
  (equal (nsubstitute 'z 'x (copy-seq '(a x b x c x d x e x f)) :count 5)
         '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2329 (:tag :sacla)
 (assert-true
  (equal (nsubstitute 'z 'x (copy-seq '(a x b x c x d x e x f)) :count 6)
         '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2330 (:tag :sacla)
 (assert-true
  (equal (nsubstitute 'z 'x (copy-seq '(a x b x c x d x e x f)) :count 7)
         '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2331 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute 'z
                'x
                (copy-seq '(a x b x c x d x e x f))
                :count nil
                :from-end t)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2332 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute 'z 'x (copy-seq '(a x b x c x d x e x f)) :count 0 :from-end t)
   '(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2333 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute 'z
                'x
                (copy-seq '(a x b x c x d x e x f))
                :count -100
                :from-end t)
   '(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2334 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute 'z 'x (copy-seq '(a x b x c x d x e x f)) :count 1 :from-end t)
   '(a x b x c x d x e z f))))
(define-test sacla-must-sequence.2335 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute 'z 'x (copy-seq '(a x b x c x d x e x f)) :count 2 :from-end t)
   '(a x b x c x d z e z f))))
(define-test sacla-must-sequence.2336 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute 'z 'x (copy-seq '(a x b x c x d x e x f)) :count 3 :from-end t)
   '(a x b x c z d z e z f))))
(define-test sacla-must-sequence.2337 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute 'z 'x (copy-seq '(a x b x c x d x e x f)) :count 4 :from-end t)
   '(a x b z c z d z e z f))))
(define-test sacla-must-sequence.2338 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute 'z 'x (copy-seq '(a x b x c x d x e x f)) :count 5 :from-end t)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2339 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute 'z 'x (copy-seq '(a x b x c x d x e x f)) :count 6 :from-end t)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2340 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute 'z 'x (copy-seq '(a x b x c x d x e x f)) :count 7 :from-end t)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2341 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute 'z 'x (copy-seq '(a x b x c x d x e x f)) :start 2 :count 1)
   '(a x b z c x d x e x f))))
(define-test sacla-must-sequence.2342 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute 'z
                'x
                (copy-seq '(a x b x c x d x e x f))
                :start 2
                :end nil
                :count 1)
   '(a x b z c x d x e x f))))
(define-test sacla-must-sequence.2343 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute 'z
                'x
                (copy-seq '(a x b x c x d x e x f))
                :start 2
                :end 6
                :count 100)
   '(a x b z c z d x e x f))))
(define-test sacla-must-sequence.2344 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute 'z
                'x
                (copy-seq '(a x b x c x d x e x f))
                :start 2
                :end 11
                :count 100)
   '(a x b z c z d z e z f))))
(define-test sacla-must-sequence.2345 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute 'z
                'x
                (copy-seq '(a x b x c x d x e x f))
                :start 2
                :end 8
                :count 10)
   '(a x b z c z d z e x f))))
(define-test sacla-must-sequence.2346 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute 'z
                'x
                (copy-seq '(a x b x c x d x e x f))
                :start 2
                :end 8
                :count 2
                :from-end t)
   '(a x b x c z d z e x f))))
(define-test sacla-must-sequence.2347 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute #\z #\c (copy-seq '(#\a #\b #\c #\d #\e #\f)) :test #'char<)
   '(#\a #\b #\c #\z #\z #\z))))
(define-test sacla-must-sequence.2348 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute #\z
                #\c
                (copy-seq '(#\a #\b #\c #\d #\e #\f))
                :test-not (complement #'char<))
   '(#\a #\b #\c #\z #\z #\z))))
(define-test sacla-must-sequence.2349 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute "peace"
                "war"
                (copy-seq '("love" "hate" "war" "peace"))
                :test #'equal)
   '("love" "hate" "peace" "peace"))))
(define-test sacla-must-sequence.2350 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute "peace"
                "war"
                (copy-seq '("love" "hate" "war" "peace"))
                :test-not (complement #'equal))
   '("love" "hate" "peace" "peace"))))
(define-test sacla-must-sequence.2351 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute "peace"
                "war"
                (copy-seq '("war" "War" "WAr" "WAR"))
                :test #'string-equal)
   '("peace" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2352 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute "peace"
                "war"
                (copy-seq '("war" "War" "WAr" "WAR"))
                :test-not (complement #'string-equal))
   '("peace" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2353 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute "peace"
                "WAR"
                (copy-seq '("war" "War" "WAr" "WAR"))
                :test #'string=)
   '("war" "War" "WAr" "peace"))))
(define-test sacla-must-sequence.2354 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute "peace"
                "WAR"
                (copy-seq '("war" "War" "WAr" "WAR"))
                :test-not (complement #'string=))
   '("war" "War" "WAr" "peace"))))
(define-test sacla-must-sequence.2355 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute "peace"
                "WAR"
                (copy-seq '("war" "War" "WAr" "WAR"))
                :test #'string=
                :key #'string-upcase)
   '("peace" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2356 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute "peace"
                "WAR"
                (copy-seq '("war" "War" "WAr" "WAR"))
                :test-not (complement #'string=)
                :key #'string-upcase)
   '("peace" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2357 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute "peace"
                "WAR"
                (copy-seq '("war" "War" "WAr" "WAR"))
                :start 1
                :end 2
                :test #'string=
                :key #'string-upcase)
   '("war" "peace" "WAr" "WAR"))))
(define-test sacla-must-sequence.2358 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute "peace"
                "WAR"
                (copy-seq '("war" "War" "WAr" "WAR"))
                :start 1
                :end 2
                :test-not (complement #'string=)
                :key #'string-upcase)
   '("war" "peace" "WAr" "WAR"))))
(define-test sacla-must-sequence.2359 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute "peace"
                "WAR"
                (copy-seq '("war" "War" "WAr" "WAR"))
                :start 1
                :end nil
                :test #'string=
                :key #'string-upcase)
   '("war" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2360 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute "peace"
                "WAR"
                (copy-seq '("war" "War" "WAr" "WAR"))
                :start 1
                :end nil
                :test-not (complement #'string=)
                :key #'string-upcase)
   '("war" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2361 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute "peace"
                "war"
                (copy-seq '("war" "War" "WAr" "WAR"))
                :test #'string=
                :key #'string-upcase)
   '("war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2362 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute "peace"
                "war"
                (copy-seq '("war" "War" "WAr" "WAR"))
                :test-not (complement #'string=)
                :key #'string-upcase)
   '("war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2363 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute "peace"
                "WAR"
                (copy-seq '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                :start 1
                :end 7
                :count 1
                :test-not (complement #'string=)
                :key #'string-upcase)
   '("war" "peace" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2364 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute "peace"
                "WAR"
                (copy-seq '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                :start 1
                :end 7
                :count 2
                :test-not (complement #'string=)
                :key #'string-upcase)
   '("war" "peace" "peace" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2365 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute "peace"
                "WAR"
                (copy-seq '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                :start 1
                :end 7
                :count 2
                :from-end t
                :test-not (complement #'string=)
                :key #'string-upcase)
   '("war" "War" "WAr" "WAR" "war" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2366 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute "peace"
                "WAR"
                (copy-seq '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                :start 1
                :end 7
                :count 0
                :from-end t
                :test-not (complement #'string=)
                :key #'string-upcase)
   '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2367 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute "peace"
                "WAR"
                (copy-seq '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                :start 1
                :end 7
                :count -2
                :from-end t
                :test-not (complement #'string=)
                :key #'string-upcase)
   '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2368 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute "peace"
                "WAR"
                (copy-seq '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                :start 1
                :end 7
                :count nil
                :from-end t
                :test-not (complement #'string=)
                :key #'string-upcase)
   '("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2369 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute "peace"
                "WAR"
                (copy-seq '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                :start 1
                :end 7
                :count 6
                :from-end t
                :test-not (complement #'string=)
                :key #'string-upcase)
   '("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2370 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute "peace"
                "WAR"
                (copy-seq '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                :start 1
                :end 7
                :count 7
                :from-end t
                :test-not (complement #'string=)
                :key #'string-upcase)
   '("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2371 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute "peace"
                "WAR"
                (copy-seq '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                :start 1
                :end 7
                :count 100
                :from-end t
                :test-not (complement #'string=)
                :key #'string-upcase)
   '("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2372 (:tag :sacla)
 (assert-true (equalp (nsubstitute 'a 'x (copy-seq #(x y z))) #(a y z))))
(define-test sacla-must-sequence.2373 (:tag :sacla)
 (assert-true (equalp (nsubstitute 'b 'y (copy-seq #(x y z))) #(x b z))))
(define-test sacla-must-sequence.2374 (:tag :sacla)
 (assert-true (equalp (nsubstitute 'c 'z (copy-seq #(x y z))) #(x y c))))
(define-test sacla-must-sequence.2375 (:tag :sacla)
 (assert-true (equalp (nsubstitute 'a 'p (copy-seq #(x y z))) #(x y z))))
(define-test sacla-must-sequence.2376 (:tag :sacla)
 (assert-true (equalp (nsubstitute 'a 'x (copy-seq #())) #())))
(define-test sacla-must-sequence.2377 (:tag :sacla)
 (assert-true
  (equalp (nsubstitute #\x #\b (copy-seq #(#\a #\b #\c #\d #\e)) :test #'char<)
          #(#\a #\b #\x #\x #\x))))
(define-test sacla-must-sequence.2378 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute #\x
                #\b
                (copy-seq #(#\a #\b #\c #\d #\e))
                :test-not (complement #'char<))
   #(#\a #\b #\x #\x #\x))))
(define-test sacla-must-sequence.2379 (:tag :sacla)
 (assert-true
  (equalp (nsubstitute '(a) 'x (copy-seq #((x) (y) (z))) :key #'car)
          #((a) (y) (z)))))
(define-test sacla-must-sequence.2380 (:tag :sacla)
 (assert-true
  (equalp (nsubstitute 'c 'b (copy-seq #(a b a b a b a b)))
          #(a c a c a c a c))))
(define-test sacla-must-sequence.2381 (:tag :sacla)
 (assert-true (equalp (nsubstitute 'a 'b (copy-seq #(b b b))) #(a a a))))
(define-test sacla-must-sequence.2382 (:tag :sacla)
 (assert-true
  (equalp (nsubstitute 'z 'x (copy-seq #(a x b x c x d x e x f)))
          #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2383 (:tag :sacla)
 (assert-true
  (equalp (nsubstitute 'z 'x (copy-seq #(a x b x c x d x e x f)) :count nil)
          #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2384 (:tag :sacla)
 (assert-true
  (equalp (nsubstitute 'z 'x (copy-seq #(a x b x c x d x e x f)) :count 0)
          #(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2385 (:tag :sacla)
 (assert-true
  (equalp (nsubstitute 'z 'x (copy-seq #(a x b x c x d x e x f)) :count -100)
          #(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2386 (:tag :sacla)
 (assert-true
  (equalp (nsubstitute 'z 'x (copy-seq #(a x b x c x d x e x f)) :count 1)
          #(a z b x c x d x e x f))))
(define-test sacla-must-sequence.2387 (:tag :sacla)
 (assert-true
  (equalp (nsubstitute 'z 'x (copy-seq #(a x b x c x d x e x f)) :count 2)
          #(a z b z c x d x e x f))))
(define-test sacla-must-sequence.2388 (:tag :sacla)
 (assert-true
  (equalp (nsubstitute 'z 'x (copy-seq #(a x b x c x d x e x f)) :count 3)
          #(a z b z c z d x e x f))))
(define-test sacla-must-sequence.2389 (:tag :sacla)
 (assert-true
  (equalp (nsubstitute 'z 'x (copy-seq #(a x b x c x d x e x f)) :count 4)
          #(a z b z c z d z e x f))))
(define-test sacla-must-sequence.2390 (:tag :sacla)
 (assert-true
  (equalp (nsubstitute 'z 'x (copy-seq #(a x b x c x d x e x f)) :count 5)
          #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2391 (:tag :sacla)
 (assert-true
  (equalp (nsubstitute 'z 'x (copy-seq #(a x b x c x d x e x f)) :count 6)
          #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2392 (:tag :sacla)
 (assert-true
  (equalp (nsubstitute 'z 'x (copy-seq #(a x b x c x d x e x f)) :count 7)
          #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2393 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute 'z
                'x
                (copy-seq #(a x b x c x d x e x f))
                :count nil
                :from-end t)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2394 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute 'z 'x (copy-seq #(a x b x c x d x e x f)) :count 0 :from-end t)
   #(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2395 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute 'z
                'x
                (copy-seq #(a x b x c x d x e x f))
                :count -100
                :from-end t)
   #(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2396 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute 'z 'x (copy-seq #(a x b x c x d x e x f)) :count 1 :from-end t)
   #(a x b x c x d x e z f))))
(define-test sacla-must-sequence.2397 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute 'z 'x (copy-seq #(a x b x c x d x e x f)) :count 2 :from-end t)
   #(a x b x c x d z e z f))))
(define-test sacla-must-sequence.2398 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute 'z 'x (copy-seq #(a x b x c x d x e x f)) :count 3 :from-end t)
   #(a x b x c z d z e z f))))
(define-test sacla-must-sequence.2399 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute 'z 'x (copy-seq #(a x b x c x d x e x f)) :count 4 :from-end t)
   #(a x b z c z d z e z f))))
(define-test sacla-must-sequence.2400 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute 'z 'x (copy-seq #(a x b x c x d x e x f)) :count 5 :from-end t)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2401 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute 'z 'x (copy-seq #(a x b x c x d x e x f)) :count 6 :from-end t)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2402 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute 'z 'x (copy-seq #(a x b x c x d x e x f)) :count 7 :from-end t)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2403 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute 'z 'x (copy-seq #(a x b x c x d x e x f)) :start 2 :count 1)
   #(a x b z c x d x e x f))))
(define-test sacla-must-sequence.2404 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute 'z
                'x
                (copy-seq #(a x b x c x d x e x f))
                :start 2
                :end nil
                :count 1)
   #(a x b z c x d x e x f))))
(define-test sacla-must-sequence.2405 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute 'z
                'x
                (copy-seq #(a x b x c x d x e x f))
                :start 2
                :end 6
                :count 100)
   #(a x b z c z d x e x f))))
(define-test sacla-must-sequence.2406 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute 'z
                'x
                (copy-seq #(a x b x c x d x e x f))
                :start 2
                :end 11
                :count 100)
   #(a x b z c z d z e z f))))
(define-test sacla-must-sequence.2407 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute 'z
                'x
                (copy-seq #(a x b x c x d x e x f))
                :start 2
                :end 8
                :count 10)
   #(a x b z c z d z e x f))))
(define-test sacla-must-sequence.2408 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute 'z
                'x
                (copy-seq #(a x b x c x d x e x f))
                :start 2
                :end 8
                :count 2
                :from-end t)
   #(a x b x c z d z e x f))))
(define-test sacla-must-sequence.2409 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute #\z #\c (copy-seq #(#\a #\b #\c #\d #\e #\f)) :test #'char<)
   #(#\a #\b #\c #\z #\z #\z))))
(define-test sacla-must-sequence.2410 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute #\z
                #\c
                (copy-seq #(#\a #\b #\c #\d #\e #\f))
                :test-not (complement #'char<))
   #(#\a #\b #\c #\z #\z #\z))))
(define-test sacla-must-sequence.2411 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute "peace"
                "war"
                (copy-seq #("love" "hate" "war" "peace"))
                :test #'equal)
   #("love" "hate" "peace" "peace"))))
(define-test sacla-must-sequence.2412 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute "peace"
                "war"
                (copy-seq #("love" "hate" "war" "peace"))
                :test-not (complement #'equal))
   #("love" "hate" "peace" "peace"))))
(define-test sacla-must-sequence.2413 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute "peace"
                "war"
                (copy-seq #("war" "War" "WAr" "WAR"))
                :test #'string-equal)
   #("peace" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2414 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute "peace"
                "war"
                (copy-seq #("war" "War" "WAr" "WAR"))
                :test-not (complement #'string-equal))
   #("peace" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2415 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute "peace"
                "WAR"
                (copy-seq #("war" "War" "WAr" "WAR"))
                :test #'string=)
   #("war" "War" "WAr" "peace"))))
(define-test sacla-must-sequence.2416 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute "peace"
                "WAR"
                (copy-seq #("war" "War" "WAr" "WAR"))
                :test-not (complement #'string=))
   #("war" "War" "WAr" "peace"))))
(define-test sacla-must-sequence.2417 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute "peace"
                "WAR"
                (copy-seq #("war" "War" "WAr" "WAR"))
                :test #'string=
                :key #'string-upcase)
   #("peace" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2418 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute "peace"
                "WAR"
                (copy-seq #("war" "War" "WAr" "WAR"))
                :test-not (complement #'string=)
                :key #'string-upcase)
   #("peace" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2419 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute "peace"
                "WAR"
                (copy-seq #("war" "War" "WAr" "WAR"))
                :start 1
                :end 2
                :test #'string=
                :key #'string-upcase)
   #("war" "peace" "WAr" "WAR"))))
(define-test sacla-must-sequence.2420 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute "peace"
                "WAR"
                (copy-seq #("war" "War" "WAr" "WAR"))
                :start 1
                :end 2
                :test-not (complement #'string=)
                :key #'string-upcase)
   #("war" "peace" "WAr" "WAR"))))
(define-test sacla-must-sequence.2421 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute "peace"
                "WAR"
                (copy-seq #("war" "War" "WAr" "WAR"))
                :start 1
                :end nil
                :test #'string=
                :key #'string-upcase)
   #("war" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2422 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute "peace"
                "WAR"
                (copy-seq #("war" "War" "WAr" "WAR"))
                :start 1
                :end nil
                :test-not (complement #'string=)
                :key #'string-upcase)
   #("war" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2423 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute "peace"
                "war"
                (copy-seq #("war" "War" "WAr" "WAR"))
                :test #'string=
                :key #'string-upcase)
   #("war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2424 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute "peace"
                "war"
                (copy-seq #("war" "War" "WAr" "WAR"))
                :test-not (complement #'string=)
                :key #'string-upcase)
   #("war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2425 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute "peace"
                "WAR"
                (copy-seq #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                :start 1
                :end 7
                :count 1
                :test-not (complement #'string=)
                :key #'string-upcase)
   #("war" "peace" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2426 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute "peace"
                "WAR"
                (copy-seq #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                :start 1
                :end 7
                :count 2
                :test-not (complement #'string=)
                :key #'string-upcase)
   #("war" "peace" "peace" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2427 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute "peace"
                "WAR"
                (copy-seq #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                :start 1
                :end 7
                :count 2
                :from-end t
                :test-not (complement #'string=)
                :key #'string-upcase)
   #("war" "War" "WAr" "WAR" "war" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2428 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute "peace"
                "WAR"
                (copy-seq #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                :start 1
                :end 7
                :count 0
                :from-end t
                :test-not (complement #'string=)
                :key #'string-upcase)
   #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2429 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute "peace"
                "WAR"
                (copy-seq #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                :start 1
                :end 7
                :count -2
                :from-end t
                :test-not (complement #'string=)
                :key #'string-upcase)
   #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2430 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute "peace"
                "WAR"
                (copy-seq #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                :start 1
                :end 7
                :count nil
                :from-end t
                :test-not (complement #'string=)
                :key #'string-upcase)
   #("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2431 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute "peace"
                "WAR"
                (copy-seq #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                :start 1
                :end 7
                :count 6
                :from-end t
                :test-not (complement #'string=)
                :key #'string-upcase)
   #("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2432 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute "peace"
                "WAR"
                (copy-seq #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                :start 1
                :end 7
                :count 7
                :from-end t
                :test-not (complement #'string=)
                :key #'string-upcase)
   #("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2433 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute "peace"
                "WAR"
                (copy-seq #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                :start 1
                :end 7
                :count 100
                :from-end t
                :test-not (complement #'string=)
                :key #'string-upcase)
   #("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2434 (:tag :sacla)
 (assert-true (string= (nsubstitute #\A #\a (copy-seq "abcabc")) "AbcAbc")))
(define-test sacla-must-sequence.2435 (:tag :sacla)
 (assert-true (string= (nsubstitute #\A #\a (copy-seq "")) "")))
(define-test sacla-must-sequence.2436 (:tag :sacla)
 (assert-true (string= (nsubstitute #\A #\a (copy-seq "xyz")) "xyz")))
(define-test sacla-must-sequence.2437 (:tag :sacla)
 (assert-true
  (string= (nsubstitute #\A #\a (copy-seq "aaaaaaaaaa") :start 5 :end nil)
           "aaaaaAAAAA")))
(define-test sacla-must-sequence.2438 (:tag :sacla)
 (assert-true
  (string= (nsubstitute #\x #\5 (copy-seq "0123456789") :test #'char<)
           "012345xxxx")))
(define-test sacla-must-sequence.2439 (:tag :sacla)
 (assert-true
  (string= (nsubstitute #\x #\5 (copy-seq "0123456789") :test #'char>)
           "xxxxx56789")))
(define-test sacla-must-sequence.2440 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute #\x #\D (copy-seq "abcdefg") :key #'char-upcase :test #'char>)
   "xxxdefg")))
(define-test sacla-must-sequence.2441 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute #\x
                #\D
                (copy-seq "abcdefg")
                :start 1
                :end 2
                :key #'char-upcase
                :test #'char>)
   "axcdefg")))
(define-test sacla-must-sequence.2442 (:tag :sacla)
 (assert-true
  (string= (nsubstitute #\A #\a (copy-seq "aaaaaaaaaa") :count 2)
           "AAaaaaaaaa")))
(define-test sacla-must-sequence.2443 (:tag :sacla)
 (assert-true
  (string= (nsubstitute #\A #\a (copy-seq "aaaaaaaaaa") :count -1)
           "aaaaaaaaaa")))
(define-test sacla-must-sequence.2444 (:tag :sacla)
 (assert-true
  (string= (nsubstitute #\A #\a (copy-seq "aaaaaaaaaa") :count 0)
           "aaaaaaaaaa")))
(define-test sacla-must-sequence.2445 (:tag :sacla)
 (assert-true
  (string= (nsubstitute #\A #\a (copy-seq "aaaaaaaaaa") :count nil)
           "AAAAAAAAAA")))
(define-test sacla-must-sequence.2446 (:tag :sacla)
 (assert-true
  (string= (nsubstitute #\A #\a (copy-seq "aaaaaaaaaa") :count 100)
           "AAAAAAAAAA")))
(define-test sacla-must-sequence.2447 (:tag :sacla)
 (assert-true
  (string= (nsubstitute #\A #\a (copy-seq "aaaaaaaaaa") :count 9)
           "AAAAAAAAAa")))
(define-test sacla-must-sequence.2448 (:tag :sacla)
 (assert-true
  (string= (nsubstitute #\A #\a (copy-seq "aaaaaaaaaa") :count 9 :from-end t)
           "aAAAAAAAAA")))
(define-test sacla-must-sequence.2449 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute #\A #\a (copy-seq "aaaaaaaaaa") :start 2 :end 8 :count 3)
   "aaAAAaaaaa")))
(define-test sacla-must-sequence.2450 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute #\A
                #\a
                (copy-seq "aaaaaaaaaa")
                :start 2
                :end 8
                :from-end t
                :count 3)
   "aaaaaAAAaa")))
(define-test sacla-must-sequence.2451 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute #\x
                #\A
                (copy-seq "aaaaaaaaaa")
                :start 2
                :end 8
                :from-end t
                :count 3)
   "aaaaaaaaaa")))
(define-test sacla-must-sequence.2452 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute #\X
                #\A
                (copy-seq "aaaaaaaaaa")
                :start 2
                :end 8
                :from-end t
                :key #'char-upcase
                :count 3)
   "aaaaaXXXaa")))
(define-test sacla-must-sequence.2453 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute #\X
                #\D
                (copy-seq "abcdefghij")
                :start 2
                :end 8
                :from-end t
                :key #'char-upcase
                :test #'char<
                :count 3)
   "abcdeXXXij")))
(define-test sacla-must-sequence.2454 (:tag :sacla)
 (assert-true (equalp (nsubstitute 0 1 (copy-seq #*1111)) #*0000)))
(define-test sacla-must-sequence.2455 (:tag :sacla)
 (assert-true
  (equalp (nsubstitute 0 1 (copy-seq #*1111) :start 1 :end nil) #*1000)))
(define-test sacla-must-sequence.2456 (:tag :sacla)
 (assert-true
  (equalp (nsubstitute 0 1 (copy-seq #*1111) :start 1 :end 3) #*1001)))
(define-test sacla-must-sequence.2457 (:tag :sacla)
 (assert-true
  (equalp (nsubstitute 0 1 (copy-seq #*11111111) :start 1 :end 7) #*10000001)))
(define-test sacla-must-sequence.2458 (:tag :sacla)
 (assert-true
  (equalp (nsubstitute 0 1 (copy-seq #*11111111) :start 1 :end 7 :count 3)
          #*10001111)))
(define-test sacla-must-sequence.2459 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute 0 1 (copy-seq #*11111111) :start 1 :end 7 :count 3 :from-end t)
   #*11110001)))
(define-test sacla-must-sequence.2460 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute 1
                1
                (copy-seq #*10101010)
                :start 1
                :end 7
                :count 3
                :from-end t
                :key #'(lambda (x) (if (zerop x) 1 0)))
   #*11111110)))
(define-test sacla-must-sequence.2461 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute 1
                1
                (copy-seq #*10101010)
                :start 1
                :end 7
                :count 3
                :from-end t
                :key #'(lambda (x) (if (zerop x) 1 0))
                :test #'>=)
   #*10101110)))
(define-test sacla-must-sequence.2462 (:tag :sacla)
 (assert-true
  (equal (nsubstitute-if 'a #'(lambda (arg) (eq arg 'x)) (copy-seq '(x y z)))
         '(a y z))))
(define-test sacla-must-sequence.2463 (:tag :sacla)
 (assert-true
  (equal (nsubstitute-if 'b #'(lambda (arg) (eq arg 'y)) (copy-seq '(x y z)))
         '(x b z))))
(define-test sacla-must-sequence.2464 (:tag :sacla)
 (assert-true
  (equal (nsubstitute-if 'c #'(lambda (arg) (eq arg 'z)) (copy-seq '(x y z)))
         '(x y c))))
(define-test sacla-must-sequence.2465 (:tag :sacla)
 (assert-true
  (equal (nsubstitute-if 'a #'(lambda (arg) (eq arg 'p)) (copy-seq '(x y z)))
         '(x y z))))
(define-test sacla-must-sequence.2466 (:tag :sacla)
 (assert-true
  (equal (nsubstitute-if 'a #'(lambda (arg) (eq arg 'x)) (copy-seq 'nil))
         'nil)))
(define-test sacla-must-sequence.2467 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if #\x
                   #'(lambda (arg) (char< #\b arg))
                   (copy-seq '(#\a #\b #\c #\d #\e)))
   '(#\a #\b #\x #\x #\x))))
(define-test sacla-must-sequence.2468 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if '(a)
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq '((x) (y) (z)))
                   :key #'car)
   '((a) (y) (z)))))
(define-test sacla-must-sequence.2469 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if 'c
                   #'(lambda (arg) (eq arg 'b))
                   (copy-seq '(a b a b a b a b)))
   '(a c a c a c a c))))
(define-test sacla-must-sequence.2470 (:tag :sacla)
 (assert-true
  (equal (nsubstitute-if 'a #'(lambda (arg) (eq arg 'b)) (copy-seq '(b b b)))
         '(a a a))))
(define-test sacla-must-sequence.2471 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq '(a x b x c x d x e x f)))
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2472 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq '(a x b x c x d x e x f))
                   :count nil)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2473 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq '(a x b x c x d x e x f))
                   :count 0)
   '(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2474 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq '(a x b x c x d x e x f))
                   :count -100)
   '(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2475 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq '(a x b x c x d x e x f))
                   :count 1)
   '(a z b x c x d x e x f))))
(define-test sacla-must-sequence.2476 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq '(a x b x c x d x e x f))
                   :count 2)
   '(a z b z c x d x e x f))))
(define-test sacla-must-sequence.2477 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq '(a x b x c x d x e x f))
                   :count 3)
   '(a z b z c z d x e x f))))
(define-test sacla-must-sequence.2478 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq '(a x b x c x d x e x f))
                   :count 4)
   '(a z b z c z d z e x f))))
(define-test sacla-must-sequence.2479 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq '(a x b x c x d x e x f))
                   :count 5)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2480 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq '(a x b x c x d x e x f))
                   :count 6)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2481 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq '(a x b x c x d x e x f))
                   :count 7)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2482 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq '(a x b x c x d x e x f))
                   :count nil
                   :from-end t)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2483 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq '(a x b x c x d x e x f))
                   :count 0
                   :from-end t)
   '(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2484 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq '(a x b x c x d x e x f))
                   :count -100
                   :from-end t)
   '(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2485 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq '(a x b x c x d x e x f))
                   :count 1
                   :from-end t)
   '(a x b x c x d x e z f))))
(define-test sacla-must-sequence.2486 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq '(a x b x c x d x e x f))
                   :count 2
                   :from-end t)
   '(a x b x c x d z e z f))))
(define-test sacla-must-sequence.2487 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq '(a x b x c x d x e x f))
                   :count 3
                   :from-end t)
   '(a x b x c z d z e z f))))
(define-test sacla-must-sequence.2488 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq '(a x b x c x d x e x f))
                   :count 4
                   :from-end t)
   '(a x b z c z d z e z f))))
(define-test sacla-must-sequence.2489 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq '(a x b x c x d x e x f))
                   :count 5
                   :from-end t)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2490 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq '(a x b x c x d x e x f))
                   :count 6
                   :from-end t)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2491 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq '(a x b x c x d x e x f))
                   :count 7
                   :from-end t)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2492 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq '(a x b x c x d x e x f))
                   :start 2
                   :count 1)
   '(a x b z c x d x e x f))))
(define-test sacla-must-sequence.2493 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq '(a x b x c x d x e x f))
                   :start 2
                   :end nil
                   :count 1)
   '(a x b z c x d x e x f))))
(define-test sacla-must-sequence.2494 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq '(a x b x c x d x e x f))
                   :start 2
                   :end 6
                   :count 100)
   '(a x b z c z d x e x f))))
(define-test sacla-must-sequence.2495 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq '(a x b x c x d x e x f))
                   :start 2
                   :end 11
                   :count 100)
   '(a x b z c z d z e z f))))
(define-test sacla-must-sequence.2496 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq '(a x b x c x d x e x f))
                   :start 2
                   :end 8
                   :count 10)
   '(a x b z c z d z e x f))))
(define-test sacla-must-sequence.2497 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq '(a x b x c x d x e x f))
                   :start 2
                   :end 8
                   :count 2
                   :from-end t)
   '(a x b x c z d z e x f))))
(define-test sacla-must-sequence.2498 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if #\z
                   #'(lambda (arg) (char< #\c arg))
                   (copy-seq '(#\a #\b #\c #\d #\e #\f)))
   '(#\a #\b #\c #\z #\z #\z))))
(define-test sacla-must-sequence.2499 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if "peace"
                   #'(lambda (arg) (equal "war" arg))
                   (copy-seq '("love" "hate" "war" "peace")))
   '("love" "hate" "peace" "peace"))))
(define-test sacla-must-sequence.2500 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if "peace"
                   #'(lambda (arg) (string-equal "war" arg))
                   (copy-seq '("war" "War" "WAr" "WAR")))
   '("peace" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2501 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if "peace"
                   #'(lambda (arg) (string= "WAR" arg))
                   (copy-seq '("war" "War" "WAr" "WAR"))
                   :key #'string-upcase)
   '("peace" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2502 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if "peace"
                   #'(lambda (arg) (string= "WAR" arg))
                   (copy-seq '("war" "War" "WAr" "WAR"))
                   :start 1
                   :end 2
                   :key #'string-upcase)
   '("war" "peace" "WAr" "WAR"))))
(define-test sacla-must-sequence.2503 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if "peace"
                   #'(lambda (arg) (string= "WAR" arg))
                   (copy-seq '("war" "War" "WAr" "WAR"))
                   :start 1
                   :end nil
                   :key #'string-upcase)
   '("war" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2504 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if "peace"
                   #'(lambda (arg) (string= "war" arg))
                   (copy-seq '("war" "War" "WAr" "WAR"))
                   :key #'string-upcase)
   '("war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2505 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if "peace"
                   #'(lambda (arg) (string= "WAR" arg))
                   (copy-seq
                    '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                   :start 1
                   :end 7
                   :count 1
                   :key #'string-upcase)
   '("war" "peace" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2506 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if "peace"
                   #'(lambda (arg) (string= "WAR" arg))
                   (copy-seq
                    '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                   :start 1
                   :end 7
                   :count 2
                   :key #'string-upcase)
   '("war" "peace" "peace" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2507 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if "peace"
                   #'(lambda (arg) (string= "WAR" arg))
                   (copy-seq
                    '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                   :start 1
                   :end 7
                   :count 2
                   :from-end t
                   :key #'string-upcase)
   '("war" "War" "WAr" "WAR" "war" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2508 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if "peace"
                   #'(lambda (arg) (string= "WAR" arg))
                   (copy-seq
                    '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                   :start 1
                   :end 7
                   :count 0
                   :from-end t
                   :key #'string-upcase)
   '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2509 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if "peace"
                   #'(lambda (arg) (string= "WAR" arg))
                   (copy-seq
                    '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                   :start 1
                   :end 7
                   :count -2
                   :from-end t
                   :key #'string-upcase)
   '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2510 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if "peace"
                   #'(lambda (arg) (string= "WAR" arg))
                   (copy-seq
                    '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                   :start 1
                   :end 7
                   :count nil
                   :from-end t
                   :key #'string-upcase)
   '("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2511 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if "peace"
                   #'(lambda (arg) (string= "WAR" arg))
                   (copy-seq
                    '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                   :start 1
                   :end 7
                   :count 6
                   :from-end t
                   :key #'string-upcase)
   '("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2512 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if "peace"
                   #'(lambda (arg) (string= "WAR" arg))
                   (copy-seq
                    '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                   :start 1
                   :end 7
                   :count 7
                   :from-end t
                   :key #'string-upcase)
   '("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2513 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if "peace"
                   #'(lambda (arg) (string= "WAR" arg))
                   (copy-seq
                    '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                   :start 1
                   :end 7
                   :count 100
                   :from-end t
                   :key #'string-upcase)
   '("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2514 (:tag :sacla)
 (assert-true
  (equalp (nsubstitute-if 'a #'(lambda (arg) (eq arg 'x)) (copy-seq #(x y z)))
          #(a y z))))
(define-test sacla-must-sequence.2515 (:tag :sacla)
 (assert-true
  (equalp (nsubstitute-if 'b #'(lambda (arg) (eq arg 'y)) (copy-seq #(x y z)))
          #(x b z))))
(define-test sacla-must-sequence.2516 (:tag :sacla)
 (assert-true
  (equalp (nsubstitute-if 'c #'(lambda (arg) (eq arg 'z)) (copy-seq #(x y z)))
          #(x y c))))
(define-test sacla-must-sequence.2517 (:tag :sacla)
 (assert-true
  (equalp (nsubstitute-if 'a #'(lambda (arg) (eq arg 'p)) (copy-seq #(x y z)))
          #(x y z))))
(define-test sacla-must-sequence.2518 (:tag :sacla)
 (assert-true
  (equalp (nsubstitute-if 'a #'(lambda (arg) (eq arg 'x)) (copy-seq #())) #())))
(define-test sacla-must-sequence.2519 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if #\x
                   #'(lambda (arg) (char< #\b arg))
                   (copy-seq #(#\a #\b #\c #\d #\e)))
   #(#\a #\b #\x #\x #\x))))
(define-test sacla-must-sequence.2520 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if '(a)
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq #((x) (y) (z)))
                   :key #'car)
   #((a) (y) (z)))))
(define-test sacla-must-sequence.2521 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 'c
                   #'(lambda (arg) (eq arg 'b))
                   (copy-seq #(a b a b a b a b)))
   #(a c a c a c a c))))
(define-test sacla-must-sequence.2522 (:tag :sacla)
 (assert-true
  (equalp (nsubstitute-if 'a #'(lambda (arg) (eq arg 'b)) (copy-seq #(b b b)))
          #(a a a))))
(define-test sacla-must-sequence.2523 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq #(a x b x c x d x e x f)))
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2524 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq #(a x b x c x d x e x f))
                   :count nil)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2525 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq #(a x b x c x d x e x f))
                   :count 0)
   #(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2526 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq #(a x b x c x d x e x f))
                   :count -100)
   #(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2527 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq #(a x b x c x d x e x f))
                   :count 1)
   #(a z b x c x d x e x f))))
(define-test sacla-must-sequence.2528 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq #(a x b x c x d x e x f))
                   :count 2)
   #(a z b z c x d x e x f))))
(define-test sacla-must-sequence.2529 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq #(a x b x c x d x e x f))
                   :count 3)
   #(a z b z c z d x e x f))))
(define-test sacla-must-sequence.2530 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq #(a x b x c x d x e x f))
                   :count 4)
   #(a z b z c z d z e x f))))
(define-test sacla-must-sequence.2531 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq #(a x b x c x d x e x f))
                   :count 5)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2532 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq #(a x b x c x d x e x f))
                   :count 6)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2533 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq #(a x b x c x d x e x f))
                   :count 7)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2534 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq #(a x b x c x d x e x f))
                   :count nil
                   :from-end t)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2535 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq #(a x b x c x d x e x f))
                   :count 0
                   :from-end t)
   #(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2536 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq #(a x b x c x d x e x f))
                   :count -100
                   :from-end t)
   #(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2537 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq #(a x b x c x d x e x f))
                   :count 1
                   :from-end t)
   #(a x b x c x d x e z f))))
(define-test sacla-must-sequence.2538 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq #(a x b x c x d x e x f))
                   :count 2
                   :from-end t)
   #(a x b x c x d z e z f))))
(define-test sacla-must-sequence.2539 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq #(a x b x c x d x e x f))
                   :count 3
                   :from-end t)
   #(a x b x c z d z e z f))))
(define-test sacla-must-sequence.2540 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq #(a x b x c x d x e x f))
                   :count 4
                   :from-end t)
   #(a x b z c z d z e z f))))
(define-test sacla-must-sequence.2541 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq #(a x b x c x d x e x f))
                   :count 5
                   :from-end t)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2542 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq #(a x b x c x d x e x f))
                   :count 6
                   :from-end t)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2543 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq #(a x b x c x d x e x f))
                   :count 7
                   :from-end t)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2544 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq #(a x b x c x d x e x f))
                   :start 2
                   :count 1)
   #(a x b z c x d x e x f))))
(define-test sacla-must-sequence.2545 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq #(a x b x c x d x e x f))
                   :start 2
                   :end nil
                   :count 1)
   #(a x b z c x d x e x f))))
(define-test sacla-must-sequence.2546 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq #(a x b x c x d x e x f))
                   :start 2
                   :end 6
                   :count 100)
   #(a x b z c z d x e x f))))
(define-test sacla-must-sequence.2547 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq #(a x b x c x d x e x f))
                   :start 2
                   :end 11
                   :count 100)
   #(a x b z c z d z e z f))))
(define-test sacla-must-sequence.2548 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq #(a x b x c x d x e x f))
                   :start 2
                   :end 8
                   :count 10)
   #(a x b z c z d z e x f))))
(define-test sacla-must-sequence.2549 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 'z
                   #'(lambda (arg) (eq arg 'x))
                   (copy-seq #(a x b x c x d x e x f))
                   :start 2
                   :end 8
                   :count 2
                   :from-end t)
   #(a x b x c z d z e x f))))
(define-test sacla-must-sequence.2550 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if #\z
                   #'(lambda (arg) (char< #\c arg))
                   (copy-seq #(#\a #\b #\c #\d #\e #\f)))
   #(#\a #\b #\c #\z #\z #\z))))
(define-test sacla-must-sequence.2551 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if "peace"
                   #'(lambda (arg) (equal "war" arg))
                   (copy-seq #("love" "hate" "war" "peace")))
   #("love" "hate" "peace" "peace"))))
(define-test sacla-must-sequence.2552 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if "peace"
                   #'(lambda (arg) (string-equal "war" arg))
                   (copy-seq #("war" "War" "WAr" "WAR")))
   #("peace" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2553 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if "peace"
                   #'(lambda (arg) (string= "WAR" arg))
                   (copy-seq #("war" "War" "WAr" "WAR"))
                   :key #'string-upcase)
   #("peace" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2554 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if "peace"
                   #'(lambda (arg) (string= "WAR" arg))
                   (copy-seq #("war" "War" "WAr" "WAR"))
                   :start 1
                   :end 2
                   :key #'string-upcase)
   #("war" "peace" "WAr" "WAR"))))
(define-test sacla-must-sequence.2555 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if "peace"
                   #'(lambda (arg) (string= "WAR" arg))
                   (copy-seq #("war" "War" "WAr" "WAR"))
                   :start 1
                   :end nil
                   :key #'string-upcase)
   #("war" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2556 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if "peace"
                   #'(lambda (arg) (string= "war" arg))
                   (copy-seq #("war" "War" "WAr" "WAR"))
                   :key #'string-upcase)
   #("war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2557 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if "peace"
                   #'(lambda (arg) (string= "WAR" arg))
                   (copy-seq
                    #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                   :start 1
                   :end 7
                   :count 1
                   :key #'string-upcase)
   #("war" "peace" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2558 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if "peace"
                   #'(lambda (arg) (string= "WAR" arg))
                   (copy-seq
                    #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                   :start 1
                   :end 7
                   :count 2
                   :key #'string-upcase)
   #("war" "peace" "peace" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2559 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if "peace"
                   #'(lambda (arg) (string= "WAR" arg))
                   (copy-seq
                    #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                   :start 1
                   :end 7
                   :count 2
                   :from-end t
                   :key #'string-upcase)
   #("war" "War" "WAr" "WAR" "war" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2560 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if "peace"
                   #'(lambda (arg) (string= "WAR" arg))
                   (copy-seq
                    #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                   :start 1
                   :end 7
                   :count 0
                   :from-end t
                   :key #'string-upcase)
   #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2561 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if "peace"
                   #'(lambda (arg) (string= "WAR" arg))
                   (copy-seq
                    #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                   :start 1
                   :end 7
                   :count -2
                   :from-end t
                   :key #'string-upcase)
   #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2562 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if "peace"
                   #'(lambda (arg) (string= "WAR" arg))
                   (copy-seq
                    #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                   :start 1
                   :end 7
                   :count nil
                   :from-end t
                   :key #'string-upcase)
   #("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2563 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if "peace"
                   #'(lambda (arg) (string= "WAR" arg))
                   (copy-seq
                    #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                   :start 1
                   :end 7
                   :count 6
                   :from-end t
                   :key #'string-upcase)
   #("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2564 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if "peace"
                   #'(lambda (arg) (string= "WAR" arg))
                   (copy-seq
                    #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                   :start 1
                   :end 7
                   :count 7
                   :from-end t
                   :key #'string-upcase)
   #("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2565 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if "peace"
                   #'(lambda (arg) (string= "WAR" arg))
                   (copy-seq
                    #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                   :start 1
                   :end 7
                   :count 100
                   :from-end t
                   :key #'string-upcase)
   #("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2566 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if #\A #'(lambda (arg) (eql #\a arg)) (copy-seq "abcabc"))
   "AbcAbc")))
(define-test sacla-must-sequence.2567 (:tag :sacla)
 (assert-true
  (string= (nsubstitute-if #\A #'(lambda (arg) (eql #\a arg)) (copy-seq ""))
           "")))
(define-test sacla-must-sequence.2568 (:tag :sacla)
 (assert-true
  (string= (nsubstitute-if #\A #'(lambda (arg) (eql #\a arg)) (copy-seq "xyz"))
           "xyz")))
(define-test sacla-must-sequence.2569 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if #\A
                   #'(lambda (arg) (eql #\a arg))
                   (copy-seq "aaaaaaaaaa")
                   :start 5
                   :end nil)
   "aaaaaAAAAA")))
(define-test sacla-must-sequence.2570 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if #\x
                   #'(lambda (arg) (char< #\5 arg))
                   (copy-seq "0123456789"))
   "012345xxxx")))
(define-test sacla-must-sequence.2571 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if #\x
                   #'(lambda (arg) (char> #\5 arg))
                   (copy-seq "0123456789"))
   "xxxxx56789")))
(define-test sacla-must-sequence.2572 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if #\x
                   #'(lambda (arg) (char> #\D arg))
                   (copy-seq "abcdefg")
                   :key #'char-upcase)
   "xxxdefg")))
(define-test sacla-must-sequence.2573 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if #\x
                   #'(lambda (arg) (char> #\D arg))
                   (copy-seq "abcdefg")
                   :start 1
                   :end 2
                   :key #'char-upcase)
   "axcdefg")))
(define-test sacla-must-sequence.2574 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if #\A
                   #'(lambda (arg) (eql #\a arg))
                   (copy-seq "aaaaaaaaaa")
                   :count 2)
   "AAaaaaaaaa")))
(define-test sacla-must-sequence.2575 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if #\A
                   #'(lambda (arg) (eql #\a arg))
                   (copy-seq "aaaaaaaaaa")
                   :count -1)
   "aaaaaaaaaa")))
(define-test sacla-must-sequence.2576 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if #\A
                   #'(lambda (arg) (eql #\a arg))
                   (copy-seq "aaaaaaaaaa")
                   :count 0)
   "aaaaaaaaaa")))
(define-test sacla-must-sequence.2577 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if #\A
                   #'(lambda (arg) (eql #\a arg))
                   (copy-seq "aaaaaaaaaa")
                   :count nil)
   "AAAAAAAAAA")))
(define-test sacla-must-sequence.2578 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if #\A
                   #'(lambda (arg) (eql #\a arg))
                   (copy-seq "aaaaaaaaaa")
                   :count 100)
   "AAAAAAAAAA")))
(define-test sacla-must-sequence.2579 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if #\A
                   #'(lambda (arg) (eql #\a arg))
                   (copy-seq "aaaaaaaaaa")
                   :count 9)
   "AAAAAAAAAa")))
(define-test sacla-must-sequence.2580 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if #\A
                   #'(lambda (arg) (eql #\a arg))
                   (copy-seq "aaaaaaaaaa")
                   :count 9
                   :from-end t)
   "aAAAAAAAAA")))
(define-test sacla-must-sequence.2581 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if #\A
                   #'(lambda (arg) (eql #\a arg))
                   (copy-seq "aaaaaaaaaa")
                   :start 2
                   :end 8
                   :count 3)
   "aaAAAaaaaa")))
(define-test sacla-must-sequence.2582 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if #\A
                   #'(lambda (arg) (eql #\a arg))
                   (copy-seq "aaaaaaaaaa")
                   :start 2
                   :end 8
                   :from-end t
                   :count 3)
   "aaaaaAAAaa")))
(define-test sacla-must-sequence.2583 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if #\x
                   #'(lambda (arg) (eql #\A arg))
                   (copy-seq "aaaaaaaaaa")
                   :start 2
                   :end 8
                   :from-end t
                   :count 3)
   "aaaaaaaaaa")))
(define-test sacla-must-sequence.2584 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if #\X
                   #'(lambda (arg) (eql #\A arg))
                   (copy-seq "aaaaaaaaaa")
                   :start 2
                   :end 8
                   :from-end t
                   :key #'char-upcase
                   :count 3)
   "aaaaaXXXaa")))
(define-test sacla-must-sequence.2585 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if #\X
                   #'(lambda (arg) (char< #\D arg))
                   (copy-seq "abcdefghij")
                   :start 2
                   :end 8
                   :from-end t
                   :key #'char-upcase
                   :count 3)
   "abcdeXXXij")))
(define-test sacla-must-sequence.2586 (:tag :sacla)
 (assert-true
  (equalp (nsubstitute-if 0 #'(lambda (arg) (= 1 arg)) (copy-seq #*1111))
          #*0000)))
(define-test sacla-must-sequence.2587 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 0
                   #'(lambda (arg) (= 1 arg))
                   (copy-seq #*1111)
                   :start 1
                   :end nil)
   #*1000)))
(define-test sacla-must-sequence.2588 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 0
                   #'(lambda (arg) (= 1 arg))
                   (copy-seq #*1111)
                   :start 1
                   :end 3)
   #*1001)))
(define-test sacla-must-sequence.2589 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 0
                   #'(lambda (arg) (= 1 arg))
                   (copy-seq #*11111111)
                   :start 1
                   :end 7)
   #*10000001)))
(define-test sacla-must-sequence.2590 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 0
                   #'(lambda (arg) (= 1 arg))
                   (copy-seq #*11111111)
                   :start 1
                   :end 7
                   :count 3)
   #*10001111)))
(define-test sacla-must-sequence.2591 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 0
                   #'(lambda (arg) (= 1 arg))
                   (copy-seq #*11111111)
                   :start 1
                   :end 7
                   :count 3
                   :from-end t)
   #*11110001)))
(define-test sacla-must-sequence.2592 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 1
                   #'(lambda (arg) (= 1 arg))
                   (copy-seq #*10101010)
                   :start 1
                   :end 7
                   :count 3
                   :from-end t
                   :key #'(lambda (x) (if (zerop x) 1 0)))
   #*11111110)))
(define-test sacla-must-sequence.2593 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if 1
                   #'(lambda (arg) (>= 1 arg))
                   (copy-seq #*10101010)
                   :start 1
                   :end 7
                   :count 3
                   :from-end t
                   :key #'(lambda (x) (if (zerop x) 1 0)))
   #*10101110)))
(define-test sacla-must-sequence.2594 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'a
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq '(x y z)))
   '(a y z))))
(define-test sacla-must-sequence.2595 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'b
                       #'(lambda (arg) (not (eq arg 'y)))
                       (copy-seq '(x y z)))
   '(x b z))))
(define-test sacla-must-sequence.2596 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'c
                       #'(lambda (arg) (not (eq arg 'z)))
                       (copy-seq '(x y z)))
   '(x y c))))
(define-test sacla-must-sequence.2597 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'a
                       #'(lambda (arg) (not (eq arg 'p)))
                       (copy-seq '(x y z)))
   '(x y z))))
(define-test sacla-must-sequence.2598 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'a #'(lambda (arg) (not (eq arg 'x))) (copy-seq 'nil))
   'nil)))
(define-test sacla-must-sequence.2599 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not #\x
                       #'(lambda (arg) (not (char< #\b arg)))
                       (copy-seq '(#\a #\b #\c #\d #\e)))
   '(#\a #\b #\x #\x #\x))))
(define-test sacla-must-sequence.2600 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not '(a)
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq '((x) (y) (z)))
                       :key #'car)
   '((a) (y) (z)))))
(define-test sacla-must-sequence.2601 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'c
                       #'(lambda (arg) (not (eq arg 'b)))
                       (copy-seq '(a b a b a b a b)))
   '(a c a c a c a c))))
(define-test sacla-must-sequence.2602 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'a
                       #'(lambda (arg) (not (eq arg 'b)))
                       (copy-seq '(b b b)))
   '(a a a))))
(define-test sacla-must-sequence.2603 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq '(a x b x c x d x e x f)))
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2604 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq '(a x b x c x d x e x f))
                       :count nil)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2605 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq '(a x b x c x d x e x f))
                       :count 0)
   '(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2606 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq '(a x b x c x d x e x f))
                       :count -100)
   '(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2607 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq '(a x b x c x d x e x f))
                       :count 1)
   '(a z b x c x d x e x f))))
(define-test sacla-must-sequence.2608 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq '(a x b x c x d x e x f))
                       :count 2)
   '(a z b z c x d x e x f))))
(define-test sacla-must-sequence.2609 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq '(a x b x c x d x e x f))
                       :count 3)
   '(a z b z c z d x e x f))))
(define-test sacla-must-sequence.2610 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq '(a x b x c x d x e x f))
                       :count 4)
   '(a z b z c z d z e x f))))
(define-test sacla-must-sequence.2611 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq '(a x b x c x d x e x f))
                       :count 5)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2612 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq '(a x b x c x d x e x f))
                       :count 6)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2613 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq '(a x b x c x d x e x f))
                       :count 7)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2614 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq '(a x b x c x d x e x f))
                       :count nil
                       :from-end t)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2615 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq '(a x b x c x d x e x f))
                       :count 0
                       :from-end t)
   '(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2616 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq '(a x b x c x d x e x f))
                       :count -100
                       :from-end t)
   '(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2617 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq '(a x b x c x d x e x f))
                       :count 1
                       :from-end t)
   '(a x b x c x d x e z f))))
(define-test sacla-must-sequence.2618 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq '(a x b x c x d x e x f))
                       :count 2
                       :from-end t)
   '(a x b x c x d z e z f))))
(define-test sacla-must-sequence.2619 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq '(a x b x c x d x e x f))
                       :count 3
                       :from-end t)
   '(a x b x c z d z e z f))))
(define-test sacla-must-sequence.2620 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq '(a x b x c x d x e x f))
                       :count 4
                       :from-end t)
   '(a x b z c z d z e z f))))
(define-test sacla-must-sequence.2621 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq '(a x b x c x d x e x f))
                       :count 5
                       :from-end t)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2622 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq '(a x b x c x d x e x f))
                       :count 6
                       :from-end t)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2623 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq '(a x b x c x d x e x f))
                       :count 7
                       :from-end t)
   '(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2624 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq '(a x b x c x d x e x f))
                       :start 2
                       :count 1)
   '(a x b z c x d x e x f))))
(define-test sacla-must-sequence.2625 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq '(a x b x c x d x e x f))
                       :start 2
                       :end nil
                       :count 1)
   '(a x b z c x d x e x f))))
(define-test sacla-must-sequence.2626 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq '(a x b x c x d x e x f))
                       :start 2
                       :end 6
                       :count 100)
   '(a x b z c z d x e x f))))
(define-test sacla-must-sequence.2627 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq '(a x b x c x d x e x f))
                       :start 2
                       :end 11
                       :count 100)
   '(a x b z c z d z e z f))))
(define-test sacla-must-sequence.2628 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq '(a x b x c x d x e x f))
                       :start 2
                       :end 8
                       :count 10)
   '(a x b z c z d z e x f))))
(define-test sacla-must-sequence.2629 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq '(a x b x c x d x e x f))
                       :start 2
                       :end 8
                       :count 2
                       :from-end t)
   '(a x b x c z d z e x f))))
(define-test sacla-must-sequence.2630 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not #\z
                       #'(lambda (arg) (not (char< #\c arg)))
                       (copy-seq '(#\a #\b #\c #\d #\e #\f)))
   '(#\a #\b #\c #\z #\z #\z))))
(define-test sacla-must-sequence.2631 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not "peace"
                       #'(lambda (arg) (not (equal "war" arg)))
                       (copy-seq '("love" "hate" "war" "peace")))
   '("love" "hate" "peace" "peace"))))
(define-test sacla-must-sequence.2632 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not "peace"
                       #'(lambda (arg) (not (string-equal "war" arg)))
                       (copy-seq '("war" "War" "WAr" "WAR")))
   '("peace" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2633 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not "peace"
                       #'(lambda (arg) (not (string= "WAR" arg)))
                       (copy-seq '("war" "War" "WAr" "WAR"))
                       :key #'string-upcase)
   '("peace" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2634 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not "peace"
                       #'(lambda (arg) (not (string= "WAR" arg)))
                       (copy-seq '("war" "War" "WAr" "WAR"))
                       :start 1
                       :end 2
                       :key #'string-upcase)
   '("war" "peace" "WAr" "WAR"))))
(define-test sacla-must-sequence.2635 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not "peace"
                       #'(lambda (arg) (not (string= "WAR" arg)))
                       (copy-seq '("war" "War" "WAr" "WAR"))
                       :start 1
                       :end nil
                       :key #'string-upcase)
   '("war" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2636 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not "peace"
                       #'(lambda (arg) (not (string= "war" arg)))
                       (copy-seq '("war" "War" "WAr" "WAR"))
                       :key #'string-upcase)
   '("war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2637 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not "peace"
                       #'(lambda (arg) (not (string= "WAR" arg)))
                       (copy-seq
                        '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                       :start 1
                       :end 7
                       :count 1
                       :key #'string-upcase)
   '("war" "peace" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2638 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not "peace"
                       #'(lambda (arg) (not (string= "WAR" arg)))
                       (copy-seq
                        '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                       :start 1
                       :end 7
                       :count 2
                       :key #'string-upcase)
   '("war" "peace" "peace" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2639 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not "peace"
                       #'(lambda (arg) (not (string= "WAR" arg)))
                       (copy-seq
                        '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                       :start 1
                       :end 7
                       :count 2
                       :from-end t
                       :key #'string-upcase)
   '("war" "War" "WAr" "WAR" "war" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2640 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not "peace"
                       #'(lambda (arg) (not (string= "WAR" arg)))
                       (copy-seq
                        '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                       :start 1
                       :end 7
                       :count 0
                       :from-end t
                       :key #'string-upcase)
   '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2641 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not "peace"
                       #'(lambda (arg) (not (string= "WAR" arg)))
                       (copy-seq
                        '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                       :start 1
                       :end 7
                       :count -2
                       :from-end t
                       :key #'string-upcase)
   '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2642 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not "peace"
                       #'(lambda (arg) (not (string= "WAR" arg)))
                       (copy-seq
                        '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                       :start 1
                       :end 7
                       :count nil
                       :from-end t
                       :key #'string-upcase)
   '("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2643 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not "peace"
                       #'(lambda (arg) (not (string= "WAR" arg)))
                       (copy-seq
                        '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                       :start 1
                       :end 7
                       :count 6
                       :from-end t
                       :key #'string-upcase)
   '("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2644 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not "peace"
                       #'(lambda (arg) (not (string= "WAR" arg)))
                       (copy-seq
                        '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                       :start 1
                       :end 7
                       :count 7
                       :from-end t
                       :key #'string-upcase)
   '("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2645 (:tag :sacla)
 (assert-true
  (equal
   (nsubstitute-if-not "peace"
                       #'(lambda (arg) (not (string= "WAR" arg)))
                       (copy-seq
                        '("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                       :start 1
                       :end 7
                       :count 100
                       :from-end t
                       :key #'string-upcase)
   '("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2646 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'a
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq #(x y z)))
   #(a y z))))
(define-test sacla-must-sequence.2647 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'b
                       #'(lambda (arg) (not (eq arg 'y)))
                       (copy-seq #(x y z)))
   #(x b z))))
(define-test sacla-must-sequence.2648 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'c
                       #'(lambda (arg) (not (eq arg 'z)))
                       (copy-seq #(x y z)))
   #(x y c))))
(define-test sacla-must-sequence.2649 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'a
                       #'(lambda (arg) (not (eq arg 'p)))
                       (copy-seq #(x y z)))
   #(x y z))))
(define-test sacla-must-sequence.2650 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'a #'(lambda (arg) (not (eq arg 'x))) (copy-seq #()))
   #())))
(define-test sacla-must-sequence.2651 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not #\x
                       #'(lambda (arg) (not (char< #\b arg)))
                       (copy-seq #(#\a #\b #\c #\d #\e)))
   #(#\a #\b #\x #\x #\x))))
(define-test sacla-must-sequence.2652 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not '(a)
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq #((x) (y) (z)))
                       :key #'car)
   #((a) (y) (z)))))
(define-test sacla-must-sequence.2653 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'c
                       #'(lambda (arg) (not (eq arg 'b)))
                       (copy-seq #(a b a b a b a b)))
   #(a c a c a c a c))))
(define-test sacla-must-sequence.2654 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'a
                       #'(lambda (arg) (not (eq arg 'b)))
                       (copy-seq #(b b b)))
   #(a a a))))
(define-test sacla-must-sequence.2655 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq #(a x b x c x d x e x f)))
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2656 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq #(a x b x c x d x e x f))
                       :count nil)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2657 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq #(a x b x c x d x e x f))
                       :count 0)
   #(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2658 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq #(a x b x c x d x e x f))
                       :count -100)
   #(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2659 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq #(a x b x c x d x e x f))
                       :count 1)
   #(a z b x c x d x e x f))))
(define-test sacla-must-sequence.2660 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq #(a x b x c x d x e x f))
                       :count 2)
   #(a z b z c x d x e x f))))
(define-test sacla-must-sequence.2661 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq #(a x b x c x d x e x f))
                       :count 3)
   #(a z b z c z d x e x f))))
(define-test sacla-must-sequence.2662 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq #(a x b x c x d x e x f))
                       :count 4)
   #(a z b z c z d z e x f))))
(define-test sacla-must-sequence.2663 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq #(a x b x c x d x e x f))
                       :count 5)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2664 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq #(a x b x c x d x e x f))
                       :count 6)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2665 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq #(a x b x c x d x e x f))
                       :count 7)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2666 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq #(a x b x c x d x e x f))
                       :count nil
                       :from-end t)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2667 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq #(a x b x c x d x e x f))
                       :count 0
                       :from-end t)
   #(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2668 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq #(a x b x c x d x e x f))
                       :count -100
                       :from-end t)
   #(a x b x c x d x e x f))))
(define-test sacla-must-sequence.2669 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq #(a x b x c x d x e x f))
                       :count 1
                       :from-end t)
   #(a x b x c x d x e z f))))
(define-test sacla-must-sequence.2670 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq #(a x b x c x d x e x f))
                       :count 2
                       :from-end t)
   #(a x b x c x d z e z f))))
(define-test sacla-must-sequence.2671 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq #(a x b x c x d x e x f))
                       :count 3
                       :from-end t)
   #(a x b x c z d z e z f))))
(define-test sacla-must-sequence.2672 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq #(a x b x c x d x e x f))
                       :count 4
                       :from-end t)
   #(a x b z c z d z e z f))))
(define-test sacla-must-sequence.2673 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq #(a x b x c x d x e x f))
                       :count 5
                       :from-end t)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2674 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq #(a x b x c x d x e x f))
                       :count 6
                       :from-end t)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2675 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq #(a x b x c x d x e x f))
                       :count 7
                       :from-end t)
   #(a z b z c z d z e z f))))
(define-test sacla-must-sequence.2676 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq #(a x b x c x d x e x f))
                       :start 2
                       :count 1)
   #(a x b z c x d x e x f))))
(define-test sacla-must-sequence.2677 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq #(a x b x c x d x e x f))
                       :start 2
                       :end nil
                       :count 1)
   #(a x b z c x d x e x f))))
(define-test sacla-must-sequence.2678 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq #(a x b x c x d x e x f))
                       :start 2
                       :end 6
                       :count 100)
   #(a x b z c z d x e x f))))
(define-test sacla-must-sequence.2679 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq #(a x b x c x d x e x f))
                       :start 2
                       :end 11
                       :count 100)
   #(a x b z c z d z e z f))))
(define-test sacla-must-sequence.2680 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq #(a x b x c x d x e x f))
                       :start 2
                       :end 8
                       :count 10)
   #(a x b z c z d z e x f))))
(define-test sacla-must-sequence.2681 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 'z
                       #'(lambda (arg) (not (eq arg 'x)))
                       (copy-seq #(a x b x c x d x e x f))
                       :start 2
                       :end 8
                       :count 2
                       :from-end t)
   #(a x b x c z d z e x f))))
(define-test sacla-must-sequence.2682 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not #\z
                       #'(lambda (arg) (not (char< #\c arg)))
                       (copy-seq #(#\a #\b #\c #\d #\e #\f)))
   #(#\a #\b #\c #\z #\z #\z))))
(define-test sacla-must-sequence.2683 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not "peace"
                       #'(lambda (arg) (not (equal "war" arg)))
                       (copy-seq #("love" "hate" "war" "peace")))
   #("love" "hate" "peace" "peace"))))
(define-test sacla-must-sequence.2684 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not "peace"
                       #'(lambda (arg) (not (string-equal "war" arg)))
                       (copy-seq #("war" "War" "WAr" "WAR")))
   #("peace" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2685 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not "peace"
                       #'(lambda (arg) (not (string= "WAR" arg)))
                       (copy-seq #("war" "War" "WAr" "WAR"))
                       :key #'string-upcase)
   #("peace" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2686 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not "peace"
                       #'(lambda (arg) (not (string= "WAR" arg)))
                       (copy-seq #("war" "War" "WAr" "WAR"))
                       :start 1
                       :end 2
                       :key #'string-upcase)
   #("war" "peace" "WAr" "WAR"))))
(define-test sacla-must-sequence.2687 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not "peace"
                       #'(lambda (arg) (not (string= "WAR" arg)))
                       (copy-seq #("war" "War" "WAr" "WAR"))
                       :start 1
                       :end nil
                       :key #'string-upcase)
   #("war" "peace" "peace" "peace"))))
(define-test sacla-must-sequence.2688 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not "peace"
                       #'(lambda (arg) (not (string= "war" arg)))
                       (copy-seq #("war" "War" "WAr" "WAR"))
                       :key #'string-upcase)
   #("war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2689 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not "peace"
                       #'(lambda (arg) (not (string= "WAR" arg)))
                       (copy-seq
                        #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                       :start 1
                       :end 7
                       :count 1
                       :key #'string-upcase)
   #("war" "peace" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2690 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not "peace"
                       #'(lambda (arg) (not (string= "WAR" arg)))
                       (copy-seq
                        #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                       :start 1
                       :end 7
                       :count 2
                       :key #'string-upcase)
   #("war" "peace" "peace" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2691 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not "peace"
                       #'(lambda (arg) (not (string= "WAR" arg)))
                       (copy-seq
                        #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                       :start 1
                       :end 7
                       :count 2
                       :from-end t
                       :key #'string-upcase)
   #("war" "War" "WAr" "WAR" "war" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2692 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not "peace"
                       #'(lambda (arg) (not (string= "WAR" arg)))
                       (copy-seq
                        #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                       :start 1
                       :end 7
                       :count 0
                       :from-end t
                       :key #'string-upcase)
   #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2693 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not "peace"
                       #'(lambda (arg) (not (string= "WAR" arg)))
                       (copy-seq
                        #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                       :start 1
                       :end 7
                       :count -2
                       :from-end t
                       :key #'string-upcase)
   #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))))
(define-test sacla-must-sequence.2694 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not "peace"
                       #'(lambda (arg) (not (string= "WAR" arg)))
                       (copy-seq
                        #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                       :start 1
                       :end 7
                       :count nil
                       :from-end t
                       :key #'string-upcase)
   #("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2695 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not "peace"
                       #'(lambda (arg) (not (string= "WAR" arg)))
                       (copy-seq
                        #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                       :start 1
                       :end 7
                       :count 6
                       :from-end t
                       :key #'string-upcase)
   #("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2696 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not "peace"
                       #'(lambda (arg) (not (string= "WAR" arg)))
                       (copy-seq
                        #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                       :start 1
                       :end 7
                       :count 7
                       :from-end t
                       :key #'string-upcase)
   #("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2697 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not "peace"
                       #'(lambda (arg) (not (string= "WAR" arg)))
                       (copy-seq
                        #("war" "War" "WAr" "WAR" "war" "War" "WAr" "WAR"))
                       :start 1
                       :end 7
                       :count 100
                       :from-end t
                       :key #'string-upcase)
   #("war" "peace" "peace" "peace" "peace" "peace" "peace" "WAR"))))
(define-test sacla-must-sequence.2698 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if-not #\A
                       #'(lambda (arg) (not (eql #\a arg)))
                       (copy-seq "abcabc"))
   "AbcAbc")))
(define-test sacla-must-sequence.2699 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if-not #\A #'(lambda (arg) (not (eql #\a arg))) (copy-seq ""))
   "")))
(define-test sacla-must-sequence.2700 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if-not #\A
                       #'(lambda (arg) (not (eql #\a arg)))
                       (copy-seq "xyz"))
   "xyz")))
(define-test sacla-must-sequence.2701 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if-not #\A
                       #'(lambda (arg) (not (eql #\a arg)))
                       (copy-seq "aaaaaaaaaa")
                       :start 5
                       :end nil)
   "aaaaaAAAAA")))
(define-test sacla-must-sequence.2702 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if-not #\x
                       #'(lambda (arg) (not (char< #\5 arg)))
                       (copy-seq "0123456789"))
   "012345xxxx")))
(define-test sacla-must-sequence.2703 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if-not #\x
                       #'(lambda (arg) (not (char> #\5 arg)))
                       (copy-seq "0123456789"))
   "xxxxx56789")))
(define-test sacla-must-sequence.2704 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if-not #\x
                       #'(lambda (arg) (not (char> #\D arg)))
                       (copy-seq "abcdefg")
                       :key #'char-upcase)
   "xxxdefg")))
(define-test sacla-must-sequence.2705 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if-not #\x
                       #'(lambda (arg) (not (char> #\D arg)))
                       (copy-seq "abcdefg")
                       :start 1
                       :end 2
                       :key #'char-upcase)
   "axcdefg")))
(define-test sacla-must-sequence.2706 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if-not #\A
                       #'(lambda (arg) (not (eql #\a arg)))
                       (copy-seq "aaaaaaaaaa")
                       :count 2)
   "AAaaaaaaaa")))
(define-test sacla-must-sequence.2707 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if-not #\A
                       #'(lambda (arg) (not (eql #\a arg)))
                       (copy-seq "aaaaaaaaaa")
                       :count -1)
   "aaaaaaaaaa")))
(define-test sacla-must-sequence.2708 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if-not #\A
                       #'(lambda (arg) (not (eql #\a arg)))
                       (copy-seq "aaaaaaaaaa")
                       :count 0)
   "aaaaaaaaaa")))
(define-test sacla-must-sequence.2709 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if-not #\A
                       #'(lambda (arg) (not (eql #\a arg)))
                       (copy-seq "aaaaaaaaaa")
                       :count nil)
   "AAAAAAAAAA")))
(define-test sacla-must-sequence.2710 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if-not #\A
                       #'(lambda (arg) (not (eql #\a arg)))
                       (copy-seq "aaaaaaaaaa")
                       :count 100)
   "AAAAAAAAAA")))
(define-test sacla-must-sequence.2711 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if-not #\A
                       #'(lambda (arg) (not (eql #\a arg)))
                       (copy-seq "aaaaaaaaaa")
                       :count 9)
   "AAAAAAAAAa")))
(define-test sacla-must-sequence.2712 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if-not #\A
                       #'(lambda (arg) (not (eql #\a arg)))
                       (copy-seq "aaaaaaaaaa")
                       :count 9
                       :from-end t)
   "aAAAAAAAAA")))
(define-test sacla-must-sequence.2713 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if-not #\A
                       #'(lambda (arg) (not (eql #\a arg)))
                       (copy-seq "aaaaaaaaaa")
                       :start 2
                       :end 8
                       :count 3)
   "aaAAAaaaaa")))
(define-test sacla-must-sequence.2714 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if-not #\A
                       #'(lambda (arg) (not (eql #\a arg)))
                       (copy-seq "aaaaaaaaaa")
                       :start 2
                       :end 8
                       :from-end t
                       :count 3)
   "aaaaaAAAaa")))
(define-test sacla-must-sequence.2715 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if-not #\x
                       #'(lambda (arg) (not (eql #\A arg)))
                       (copy-seq "aaaaaaaaaa")
                       :start 2
                       :end 8
                       :from-end t
                       :count 3)
   "aaaaaaaaaa")))
(define-test sacla-must-sequence.2716 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if-not #\X
                       #'(lambda (arg) (not (eql #\A arg)))
                       (copy-seq "aaaaaaaaaa")
                       :start 2
                       :end 8
                       :from-end t
                       :key #'char-upcase
                       :count 3)
   "aaaaaXXXaa")))
(define-test sacla-must-sequence.2717 (:tag :sacla)
 (assert-true
  (string=
   (nsubstitute-if-not #\X
                       #'(lambda (arg) (not (char< #\D arg)))
                       (copy-seq "abcdefghij")
                       :start 2
                       :end 8
                       :from-end t
                       :key #'char-upcase
                       :count 3)
   "abcdeXXXij")))
(define-test sacla-must-sequence.2718 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 0 #'(lambda (arg) (not (= 1 arg))) (copy-seq #*1111))
   #*0000)))
(define-test sacla-must-sequence.2719 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 0
                       #'(lambda (arg) (not (= 1 arg)))
                       (copy-seq #*1111)
                       :start 1
                       :end nil)
   #*1000)))
(define-test sacla-must-sequence.2720 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 0
                       #'(lambda (arg) (not (= 1 arg)))
                       (copy-seq #*1111)
                       :start 1
                       :end 3)
   #*1001)))
(define-test sacla-must-sequence.2721 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 0
                       #'(lambda (arg) (not (= 1 arg)))
                       (copy-seq #*11111111)
                       :start 1
                       :end 7)
   #*10000001)))
(define-test sacla-must-sequence.2722 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 0
                       #'(lambda (arg) (not (= 1 arg)))
                       (copy-seq #*11111111)
                       :start 1
                       :end 7
                       :count 3)
   #*10001111)))
(define-test sacla-must-sequence.2723 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 0
                       #'(lambda (arg) (not (= 1 arg)))
                       (copy-seq #*11111111)
                       :start 1
                       :end 7
                       :count 3
                       :from-end t)
   #*11110001)))
(define-test sacla-must-sequence.2724 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 1
                       #'(lambda (arg) (not (= 1 arg)))
                       (copy-seq #*10101010)
                       :start 1
                       :end 7
                       :count 3
                       :from-end t
                       :key #'(lambda (x) (if (zerop x) 1 0)))
   #*11111110)))
(define-test sacla-must-sequence.2725 (:tag :sacla)
 (assert-true
  (equalp
   (nsubstitute-if-not 1
                       #'(lambda (arg) (not (>= 1 arg)))
                       (copy-seq #*10101010)
                       :start 1
                       :end 7
                       :count 3
                       :from-end t
                       :key #'(lambda (x) (if (zerop x) 1 0)))
   #*10101110)))
(define-test sacla-must-sequence.2726 (:tag :sacla)
 (assert-true
  (string= (concatenate 'string "all" " " "together" " " "now")
           "all together now")))
(define-test sacla-must-sequence.2727 (:tag :sacla)
 (assert-true
  (equal (concatenate 'list "ABC" '(d e f) #(1 2 3) #*1011)
         '(#\A #\B #\C d e f 1 2 3 1 0 1 1))))
(define-test sacla-must-sequence.2728 (:tag :sacla)
 (assert-true (null (concatenate 'list))))
(define-test sacla-must-sequence.2729 (:tag :sacla)
 (assert-true
  (handler-case
      (progn
        (concatenate 'symbol))
    (error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-sequence.2730 (:tag :sacla)
 (assert-true
  (handler-case
      (progn
        (concatenate 'class))
    (error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-sequence.2731 (:tag :sacla)
 (assert-true
  (equal (concatenate 'list 'nil '(a b c) '(x y z)) '(a b c x y z))))
(define-test sacla-must-sequence.2732 (:tag :sacla)
 (assert-true
  (equal (concatenate 'list '(a) #(b) '(c) #(x y) '(z)) '(a b c x y z))))
(define-test sacla-must-sequence.2733 (:tag :sacla)
 (assert-true
  (equal (concatenate 'list '(a b c) #(d e f) "ghi" #*0101)
         '(a b c d e f #\g #\h #\i 0 1 0 1))))
(define-test sacla-must-sequence.2734 (:tag :sacla)
 (assert-true (null (concatenate 'list))))
(define-test sacla-must-sequence.2735 (:tag :sacla)
 (assert-true
  (let* ((list0 '(a b c)) (list (concatenate 'list list0)))
    (and (not (eq list0 list)) (equal list list0) (equal list '(a b c))))))
(define-test sacla-must-sequence.2736 (:tag :sacla)
 (assert-true (null (concatenate 'list 'nil #() "" #*))))
(define-test sacla-must-sequence.2737 (:tag :sacla)
 (assert-true
  (equal (concatenate 'list #(a b c) 'nil 'nil '(x y z) #() #() #* #(j k l))
         '(a b c x y z j k l))))
(define-test sacla-must-sequence.2738 (:tag :sacla)
 (assert-true
  (equalp (concatenate 'vector 'nil '(a b c) '(x y z)) #(a b c x y z))))
(define-test sacla-must-sequence.2739 (:tag :sacla)
 (assert-true
  (equalp (concatenate 'vector '(a) #(b) '(c) #(x y) '(z)) #(a b c x y z))))
(define-test sacla-must-sequence.2740 (:tag :sacla)
 (assert-true
  (equalp (concatenate 'vector '(a b c) #(d e f) "ghi" #*0101)
          #(a b c d e f #\g #\h #\i 0 1 0 1))))
(define-test sacla-must-sequence.2741 (:tag :sacla)
 (assert-true (equalp (concatenate 'vector) #())))
(define-test sacla-must-sequence.2742 (:tag :sacla)
 (assert-true
  (let* ((vector0 #(a b c)) (vector (concatenate 'vector vector0)))
    (and (not (eq vector0 vector))
         (equalp vector vector0)
         (equalp vector #(a b c))))))
(define-test sacla-must-sequence.2743 (:tag :sacla)
 (assert-true (equalp (concatenate 'vector 'nil #() "" #*) #())))
(define-test sacla-must-sequence.2744 (:tag :sacla)
 (assert-true
  (equalp (concatenate 'vector #(a b c) 'nil 'nil '(x y z) #() #() #* #(j k l))
          #(a b c x y z j k l))))
(define-test sacla-must-sequence.2745 (:tag :sacla)
 (assert-true
  (string= (concatenate 'string "abc" "def" "ghi" "jkl" "mno" "pqr")
           "abcdefghijklmnopqr")))
(define-test sacla-must-sequence.2746 (:tag :sacla)
 (assert-true
  (string=
   (concatenate 'string
                ""
                "abc"
                ""
                "def"
                ""
                "ghi"
                ""
                ""
                "jkl"
                ""
                "mno"
                ""
                "pqr"
                ""
                "")
   "abcdefghijklmnopqr")))
(define-test sacla-must-sequence.2747 (:tag :sacla)
 (assert-true (string= (concatenate 'string) "")))
(define-test sacla-must-sequence.2748 (:tag :sacla)
 (assert-true (string= (concatenate 'string "" 'nil #* #()) "")))
(define-test sacla-must-sequence.2749 (:tag :sacla)
 (assert-true
  (string=
   (concatenate 'string "abc" '(#\d #\e #\f #\g) #(#\h #\i #\j #\k #\l))
   "abcdefghijkl")))
(define-test sacla-must-sequence.2750 (:tag :sacla)
 (assert-true (equal (concatenate 'bit-vector #*0101 #*1010) #*01011010)))
(define-test sacla-must-sequence.2751 (:tag :sacla)
 (assert-true
  (equal (concatenate 'bit-vector #*0101 #*1010 #* #*11 #*1 #*1)
         #*010110101111)))
(define-test sacla-must-sequence.2752 (:tag :sacla)
 (assert-true
  (equal (concatenate 'bit-vector '(0 1 0 1) '(0 1 0 1) #(0 1 0 1) #*0101)
         #*0101010101010101)))
(define-test sacla-must-sequence.2753 (:tag :sacla)
 (assert-true (equal (concatenate 'bit-vector) #*)))
(define-test sacla-must-sequence.2754 (:tag :sacla)
 (assert-true (equal (concatenate 'bit-vector #*) #*)))
(define-test sacla-must-sequence.2755 (:tag :sacla)
 (assert-true (equal (concatenate 'bit-vector #* 'nil #()) #*)))
(define-test sacla-must-sequence.2756 (:tag :sacla)
 (assert-true
  (let ((test1 (list 1 3 4 6 7)) (test2 (list 2 5 8)))
    (equal (merge 'list test1 test2 #'<) '(1 2 3 4 5 6 7 8)))))
(define-test sacla-must-sequence.2757 (:tag :sacla)
 (assert-true
  (let ((test1 (copy-seq "BOY")) (test2 (copy-seq "nosy")))
    (equal (merge 'string test1 test2 #'char-lessp) "BnOosYy"))))
(define-test sacla-must-sequence.2758 (:tag :sacla)
 (assert-true
  (let ((test1 (vector '(red . 1) '(blue . 4)))
        (test2 (vector '(yellow . 2) '(green . 7))))
    (equalp (merge 'vector test1 test2 #'< :key #'cdr)
            #((red . 1) (yellow . 2) (blue . 4) (green . 7))))))
(define-test sacla-must-sequence.2759 (:tag :sacla)
 (assert-true
  (equal (merge 'list (list 1 3 5 7 9) (list 0 2 4 6 8) #'<)
         '(0 1 2 3 4 5 6 7 8 9))))
(define-test sacla-must-sequence.2760 (:tag :sacla)
 (assert-true
  (equal (merge 'cons (list 1 3 5 7 9) (list 0 2 4 6 8) #'<)
         '(0 1 2 3 4 5 6 7 8 9))))
(define-test sacla-must-sequence.2761 (:tag :sacla)
 (assert-true (equal (merge 'list (list 0 1 2) nil #'<) '(0 1 2))))
(define-test sacla-must-sequence.2762 (:tag :sacla)
 (assert-true (equal (merge 'list nil (list 0 1 2) #'<) '(0 1 2))))
(define-test sacla-must-sequence.2763 (:tag :sacla)
 (assert-true (equal (merge 'list nil nil #'<) nil)))
(define-test sacla-must-sequence.2764 (:tag :sacla)
 (assert-true
  (equal
   (merge 'list
          (list '(1 1) '(2 1) '(3 1))
          (list '(1 2) '(2 2) '(3 2))
          #'<
          :key #'car)
   '((1 1) (1 2) (2 1) (2 2) (3 1) (3 2)))))
(define-test sacla-must-sequence.2765 (:tag :sacla)
 (assert-true
  (equal
   (merge 'list
          (list '(1 1) '(2 1) '(2 1 1) '(3 1))
          (list '(1 2) '(2 2) '(3 2))
          #'<
          :key #'car)
   '((1 1) (1 2) (2 1) (2 1 1) (2 2) (3 1) (3 2)))))
(define-test sacla-must-sequence.2766 (:tag :sacla)
 (assert-true
  (equal
   (merge 'list
          (list '(1 1) '(2 1) '(2 1 1) '(3 1))
          (list '(1 2) '(2 2) '(3 2) '(3 2 2))
          #'<
          :key #'car)
   '((1 1) (1 2) (2 1) (2 1 1) (2 2) (3 1) (3 2) (3 2 2)))))
(define-test sacla-must-sequence.2767 (:tag :sacla)
 (assert-true
  (equal (merge 'list (list 3 1 9 5 7) (list 8 6 0 2 4) #'<)
         '(3 1 8 6 0 2 4 9 5 7))))
(define-test sacla-must-sequence.2768 (:tag :sacla)
 (assert-true
  (equal (merge 'list (vector 1 3 5 7 9) (list 0 2 4 6 8) #'<)
         '(0 1 2 3 4 5 6 7 8 9))))
(define-test sacla-must-sequence.2769 (:tag :sacla)
 (assert-true
  (equal (merge 'cons (vector 1 3 5 7 9) (list 0 2 4 6 8) #'<)
         '(0 1 2 3 4 5 6 7 8 9))))
(define-test sacla-must-sequence.2770 (:tag :sacla)
 (assert-true (equal (merge 'list (vector 0 1 2) nil #'<) '(0 1 2))))
(define-test sacla-must-sequence.2771 (:tag :sacla)
 (assert-true (equal (merge 'list #() (list 0 1 2) #'<) '(0 1 2))))
(define-test sacla-must-sequence.2772 (:tag :sacla)
 (assert-true (equal (merge 'list #() #() #'<) nil)))
(define-test sacla-must-sequence.2773 (:tag :sacla)
 (assert-true
  (equal
   (merge 'list
          (vector '(1 1) '(2 1) '(3 1))
          (list '(1 2) '(2 2) '(3 2))
          #'<
          :key #'car)
   '((1 1) (1 2) (2 1) (2 2) (3 1) (3 2)))))
(define-test sacla-must-sequence.2774 (:tag :sacla)
 (assert-true
  (equal
   (merge 'list
          (vector '(1 1) '(2 1) '(2 1 1) '(3 1))
          (list '(1 2) '(2 2) '(3 2))
          #'<
          :key #'car)
   '((1 1) (1 2) (2 1) (2 1 1) (2 2) (3 1) (3 2)))))
(define-test sacla-must-sequence.2775 (:tag :sacla)
 (assert-true
  (equal
   (merge 'list
          (vector '(1 1) '(2 1) '(2 1 1) '(3 1))
          (list '(1 2) '(2 2) '(3 2) '(3 2 2))
          #'<
          :key #'car)
   '((1 1) (1 2) (2 1) (2 1 1) (2 2) (3 1) (3 2) (3 2 2)))))
(define-test sacla-must-sequence.2776 (:tag :sacla)
 (assert-true
  (equal (merge 'list (vector 3 1 9 5 7) (list 8 6 0 2 4) #'<)
         '(3 1 8 6 0 2 4 9 5 7))))
(define-test sacla-must-sequence.2777 (:tag :sacla)
 (assert-true
  (equal (merge 'list (list 1 3 5 7 9) (vector 0 2 4 6 8) #'<)
         '(0 1 2 3 4 5 6 7 8 9))))
(define-test sacla-must-sequence.2778 (:tag :sacla)
 (assert-true
  (equal (merge 'cons (list 1 3 5 7 9) (vector 0 2 4 6 8) #'<)
         '(0 1 2 3 4 5 6 7 8 9))))
(define-test sacla-must-sequence.2779 (:tag :sacla)
 (assert-true (equal (merge 'list (list 0 1 2) #() #'<) '(0 1 2))))
(define-test sacla-must-sequence.2780 (:tag :sacla)
 (assert-true (equal (merge 'list nil (vector 0 1 2) #'<) '(0 1 2))))
(define-test sacla-must-sequence.2781 (:tag :sacla)
 (assert-true (equal (merge 'list nil #() #'<) nil)))
(define-test sacla-must-sequence.2782 (:tag :sacla)
 (assert-true
  (equal
   (merge 'list
          (list '(1 1) '(2 1) '(3 1))
          (vector '(1 2) '(2 2) '(3 2))
          #'<
          :key #'car)
   '((1 1) (1 2) (2 1) (2 2) (3 1) (3 2)))))
(define-test sacla-must-sequence.2783 (:tag :sacla)
 (assert-true
  (equal
   (merge 'list
          (list '(1 1) '(2 1) '(2 1 1) '(3 1))
          (vector '(1 2) '(2 2) '(3 2))
          #'<
          :key #'car)
   '((1 1) (1 2) (2 1) (2 1 1) (2 2) (3 1) (3 2)))))
(define-test sacla-must-sequence.2784 (:tag :sacla)
 (assert-true
  (equal
   (merge 'list
          (list '(1 1) '(2 1) '(2 1 1) '(3 1))
          (vector '(1 2) '(2 2) '(3 2) '(3 2 2))
          #'<
          :key #'car)
   '((1 1) (1 2) (2 1) (2 1 1) (2 2) (3 1) (3 2) (3 2 2)))))
(define-test sacla-must-sequence.2785 (:tag :sacla)
 (assert-true
  (equal (merge 'list (list 3 1 9 5 7) (vector 8 6 0 2 4) #'<)
         '(3 1 8 6 0 2 4 9 5 7))))
(define-test sacla-must-sequence.2786 (:tag :sacla)
 (assert-true
  (equal (merge 'list (vector 1 3 5 7 9) (vector 0 2 4 6 8) #'<)
         '(0 1 2 3 4 5 6 7 8 9))))
(define-test sacla-must-sequence.2787 (:tag :sacla)
 (assert-true
  (equal (merge 'cons (vector 1 3 5 7 9) (vector 0 2 4 6 8) #'<)
         '(0 1 2 3 4 5 6 7 8 9))))
(define-test sacla-must-sequence.2788 (:tag :sacla)
 (assert-true (equal (merge 'list (vector 0 1 2) #() #'<) '(0 1 2))))
(define-test sacla-must-sequence.2789 (:tag :sacla)
 (assert-true (equal (merge 'list #() (vector 0 1 2) #'<) '(0 1 2))))
(define-test sacla-must-sequence.2790 (:tag :sacla)
 (assert-true (equal (merge 'list #() #() #'<) nil)))
(define-test sacla-must-sequence.2791 (:tag :sacla)
 (assert-true
  (equal
   (merge 'list
          (vector '(1 1) '(2 1) '(3 1))
          (vector '(1 2) '(2 2) '(3 2))
          #'<
          :key #'car)
   '((1 1) (1 2) (2 1) (2 2) (3 1) (3 2)))))
(define-test sacla-must-sequence.2792 (:tag :sacla)
 (assert-true
  (equal
   (merge 'list
          (vector '(1 1) '(2 1) '(2 1 1) '(3 1))
          (vector '(1 2) '(2 2) '(3 2))
          #'<
          :key #'car)
   '((1 1) (1 2) (2 1) (2 1 1) (2 2) (3 1) (3 2)))))
(define-test sacla-must-sequence.2793 (:tag :sacla)
 (assert-true
  (equal
   (merge 'list
          (vector '(1 1) '(2 1) '(2 1 1) '(3 1))
          (vector '(1 2) '(2 2) '(3 2) '(3 2 2))
          #'<
          :key #'car)
   '((1 1) (1 2) (2 1) (2 1 1) (2 2) (3 1) (3 2) (3 2 2)))))
(define-test sacla-must-sequence.2794 (:tag :sacla)
 (assert-true
  (equal (merge 'list (vector 3 1 9 5 7) (vector 8 6 0 2 4) #'<)
         '(3 1 8 6 0 2 4 9 5 7))))
(define-test sacla-must-sequence.2795 (:tag :sacla)
 (assert-true
  (equalp (merge 'vector (list 1 3 5 7 9) (list 0 2 4 6 8) #'<)
          #(0 1 2 3 4 5 6 7 8 9))))
(define-test sacla-must-sequence.2796 (:tag :sacla)
 (assert-true
  (equalp (merge 'vector (list 1 3 5 7 9) (list 0 2 4 6 8) #'<)
          #(0 1 2 3 4 5 6 7 8 9))))
(define-test sacla-must-sequence.2797 (:tag :sacla)
 (assert-true (equalp (merge 'vector (list 0 1 2) nil #'<) #(0 1 2))))
(define-test sacla-must-sequence.2798 (:tag :sacla)
 (assert-true (equalp (merge 'vector nil (list 0 1 2) #'<) #(0 1 2))))
(define-test sacla-must-sequence.2799 (:tag :sacla)
 (assert-true (equalp (merge 'vector nil nil #'<) #())))
(define-test sacla-must-sequence.2800 (:tag :sacla)
 (assert-true
  (equalp
   (merge 'vector
          (list '(1 1) '(2 1) '(3 1))
          (list '(1 2) '(2 2) '(3 2))
          #'<
          :key #'car)
   #((1 1) (1 2) (2 1) (2 2) (3 1) (3 2)))))
(define-test sacla-must-sequence.2801 (:tag :sacla)
 (assert-true
  (equalp
   (merge 'vector
          (list '(1 1) '(2 1) '(2 1 1) '(3 1))
          (list '(1 2) '(2 2) '(3 2))
          #'<
          :key #'car)
   #((1 1) (1 2) (2 1) (2 1 1) (2 2) (3 1) (3 2)))))
(define-test sacla-must-sequence.2802 (:tag :sacla)
 (assert-true
  (equalp
   (merge 'vector
          (list '(1 1) '(2 1) '(2 1 1) '(3 1))
          (list '(1 2) '(2 2) '(3 2) '(3 2 2))
          #'<
          :key #'car)
   #((1 1) (1 2) (2 1) (2 1 1) (2 2) (3 1) (3 2) (3 2 2)))))
(define-test sacla-must-sequence.2803 (:tag :sacla)
 (assert-true
  (equalp (merge 'vector (list 3 1 9 5 7) (list 8 6 0 2 4) #'<)
          #(3 1 8 6 0 2 4 9 5 7))))
(define-test sacla-must-sequence.2804 (:tag :sacla)
 (assert-true
  (equalp (merge 'vector (vector 1 3 5 7 9) (list 0 2 4 6 8) #'<)
          #(0 1 2 3 4 5 6 7 8 9))))
(define-test sacla-must-sequence.2805 (:tag :sacla)
 (assert-true
  (equalp (merge 'vector (vector 1 3 5 7 9) (list 0 2 4 6 8) #'<)
          #(0 1 2 3 4 5 6 7 8 9))))
(define-test sacla-must-sequence.2806 (:tag :sacla)
 (assert-true (equalp (merge 'vector (vector 0 1 2) nil #'<) #(0 1 2))))
(define-test sacla-must-sequence.2807 (:tag :sacla)
 (assert-true (equalp (merge 'vector #() (list 0 1 2) #'<) #(0 1 2))))
(define-test sacla-must-sequence.2808 (:tag :sacla)
 (assert-true (equalp (merge 'vector #() #() #'<) #())))
(define-test sacla-must-sequence.2809 (:tag :sacla)
 (assert-true
  (equalp
   (merge 'vector
          (vector '(1 1) '(2 1) '(3 1))
          (list '(1 2) '(2 2) '(3 2))
          #'<
          :key #'car)
   #((1 1) (1 2) (2 1) (2 2) (3 1) (3 2)))))
(define-test sacla-must-sequence.2810 (:tag :sacla)
 (assert-true
  (equalp
   (merge 'vector
          (vector '(1 1) '(2 1) '(2 1 1) '(3 1))
          (list '(1 2) '(2 2) '(3 2))
          #'<
          :key #'car)
   #((1 1) (1 2) (2 1) (2 1 1) (2 2) (3 1) (3 2)))))
(define-test sacla-must-sequence.2811 (:tag :sacla)
 (assert-true
  (equalp
   (merge 'vector
          (vector '(1 1) '(2 1) '(2 1 1) '(3 1))
          (list '(1 2) '(2 2) '(3 2) '(3 2 2))
          #'<
          :key #'car)
   #((1 1) (1 2) (2 1) (2 1 1) (2 2) (3 1) (3 2) (3 2 2)))))
(define-test sacla-must-sequence.2812 (:tag :sacla)
 (assert-true
  (equalp (merge 'vector (vector 3 1 9 5 7) (list 8 6 0 2 4) #'<)
          #(3 1 8 6 0 2 4 9 5 7))))
(define-test sacla-must-sequence.2813 (:tag :sacla)
 (assert-true
  (equalp (merge 'vector (list 1 3 5 7 9) (vector 0 2 4 6 8) #'<)
          #(0 1 2 3 4 5 6 7 8 9))))
(define-test sacla-must-sequence.2814 (:tag :sacla)
 (assert-true
  (equalp (merge 'vector (list 1 3 5 7 9) (vector 0 2 4 6 8) #'<)
          #(0 1 2 3 4 5 6 7 8 9))))
(define-test sacla-must-sequence.2815 (:tag :sacla)
 (assert-true (equalp (merge 'vector (list 0 1 2) #() #'<) #(0 1 2))))
(define-test sacla-must-sequence.2816 (:tag :sacla)
 (assert-true (equalp (merge 'vector nil (vector 0 1 2) #'<) #(0 1 2))))
(define-test sacla-must-sequence.2817 (:tag :sacla)
 (assert-true (equalp (merge 'vector nil #() #'<) #())))
(define-test sacla-must-sequence.2818 (:tag :sacla)
 (assert-true
  (equalp
   (merge 'vector
          (list '(1 1) '(2 1) '(3 1))
          (vector '(1 2) '(2 2) '(3 2))
          #'<
          :key #'car)
   #((1 1) (1 2) (2 1) (2 2) (3 1) (3 2)))))
(define-test sacla-must-sequence.2819 (:tag :sacla)
 (assert-true
  (equalp
   (merge 'vector
          (list '(1 1) '(2 1) '(2 1 1) '(3 1))
          (vector '(1 2) '(2 2) '(3 2))
          #'<
          :key #'car)
   #((1 1) (1 2) (2 1) (2 1 1) (2 2) (3 1) (3 2)))))
(define-test sacla-must-sequence.2820 (:tag :sacla)
 (assert-true
  (equalp
   (merge 'vector
          (list '(1 1) '(2 1) '(2 1 1) '(3 1))
          (vector '(1 2) '(2 2) '(3 2) '(3 2 2))
          #'<
          :key #'car)
   #((1 1) (1 2) (2 1) (2 1 1) (2 2) (3 1) (3 2) (3 2 2)))))
(define-test sacla-must-sequence.2821 (:tag :sacla)
 (assert-true
  (equalp (merge 'vector (list 3 1 9 5 7) (vector 8 6 0 2 4) #'<)
          #(3 1 8 6 0 2 4 9 5 7))))
(define-test sacla-must-sequence.2822 (:tag :sacla)
 (assert-true
  (equalp (merge 'vector (vector 1 3 5 7 9) (vector 0 2 4 6 8) #'<)
          #(0 1 2 3 4 5 6 7 8 9))))
(define-test sacla-must-sequence.2823 (:tag :sacla)
 (assert-true (equalp (merge 'vector (vector 0 1 2) #() #'<) #(0 1 2))))
(define-test sacla-must-sequence.2824 (:tag :sacla)
 (assert-true (equalp (merge 'vector #() (vector 0 1 2) #'<) #(0 1 2))))
(define-test sacla-must-sequence.2825 (:tag :sacla)
 (assert-true (equalp (merge 'vector #() #() #'<) #())))
(define-test sacla-must-sequence.2826 (:tag :sacla)
 (assert-true
  (equalp
   (merge 'vector
          (vector '(1 1) '(2 1) '(3 1))
          (vector '(1 2) '(2 2) '(3 2))
          #'<
          :key #'car)
   #((1 1) (1 2) (2 1) (2 2) (3 1) (3 2)))))
(define-test sacla-must-sequence.2827 (:tag :sacla)
 (assert-true
  (equalp
   (merge 'vector
          (vector '(1 1) '(2 1) '(2 1 1) '(3 1))
          (vector '(1 2) '(2 2) '(3 2))
          #'<
          :key #'car)
   #((1 1) (1 2) (2 1) (2 1 1) (2 2) (3 1) (3 2)))))
(define-test sacla-must-sequence.2828 (:tag :sacla)
 (assert-true
  (equalp
   (merge 'vector
          (vector '(1 1) '(2 1) '(2 1 1) '(3 1))
          (vector '(1 2) '(2 2) '(3 2) '(3 2 2))
          #'<
          :key #'car)
   #((1 1) (1 2) (2 1) (2 1 1) (2 2) (3 1) (3 2) (3 2 2)))))
(define-test sacla-must-sequence.2829 (:tag :sacla)
 (assert-true
  (equalp (merge 'vector (vector 3 1 9 5 7) (vector 8 6 0 2 4) #'<)
          #(3 1 8 6 0 2 4 9 5 7))))
(define-test sacla-must-sequence.2830 (:tag :sacla)
 (assert-true
  (string= (merge 'string (list #\a #\c #\e) (list #\b #\d #\f) #'char<)
           "abcdef")))
(define-test sacla-must-sequence.2831 (:tag :sacla)
 (assert-true
  (string= (merge 'string (list #\a #\b #\c) (list #\d #\e #\f) #'char<)
           "abcdef")))
(define-test sacla-must-sequence.2832 (:tag :sacla)
 (assert-true (string= (merge 'string (list #\a #\b #\c) 'nil #'char<) "abc")))
(define-test sacla-must-sequence.2833 (:tag :sacla)
 (assert-true (string= (merge 'string 'nil (list #\a #\b #\c) #'char<) "abc")))
(define-test sacla-must-sequence.2834 (:tag :sacla)
 (assert-true
  (string= (merge 'string (list #\a #\b #\c) (copy-seq "") #'char<) "abc")))
(define-test sacla-must-sequence.2835 (:tag :sacla)
 (assert-true
  (string= (merge 'string (list #\a #\b #\c) (copy-seq "BCD") #'char-lessp)
           "abBcCD")))
(define-test sacla-must-sequence.2836 (:tag :sacla)
 (assert-true
  (string= (merge 'string (list #\a #\b #\z) #(#\c #\x #\y) #'char<) "abcxyz")))
(define-test sacla-must-sequence.2837 (:tag :sacla)
 (assert-true
  (equal (merge 'bit-vector (copy-seq #*0101) (copy-seq #*1010) #'<)
         #*01011010)))
(define-test sacla-must-sequence.2838 (:tag :sacla)
 (assert-true
  (equal (merge 'bit-vector (copy-seq #*0101) (copy-seq #*) #'<) #*0101)))
(define-test sacla-must-sequence.2839 (:tag :sacla)
 (assert-true (equal (merge 'bit-vector (copy-seq #*0101) 'nil #'<) #*0101)))
(define-test sacla-must-sequence.2840 (:tag :sacla)
 (assert-true (equal (merge 'bit-vector nil (copy-seq #*0101) #'<) #*0101)))
(define-test sacla-must-sequence.2841 (:tag :sacla)
 (assert-true
  (equal (merge 'bit-vector (copy-seq #*0101) (copy-seq #*0101) #'<)
         #*00101101)))
(define-test sacla-must-sequence.2842 (:tag :sacla)
 (assert-true (equal (remove 4 '(1 3 4 5 9)) '(1 3 5 9))))
(define-test sacla-must-sequence.2843 (:tag :sacla)
 (assert-true (equal (remove 4 '(1 2 4 1 3 4 5)) '(1 2 1 3 5))))
(define-test sacla-must-sequence.2844 (:tag :sacla)
 (assert-true (equal (remove 4 '(1 2 4 1 3 4 5) :count 1) '(1 2 1 3 4 5))))
(define-test sacla-must-sequence.2845 (:tag :sacla)
 (assert-true
  (equal (remove 4 '(1 2 4 1 3 4 5) :count 1 :from-end t) '(1 2 4 1 3 5))))
(define-test sacla-must-sequence.2846 (:tag :sacla)
 (assert-true (equal (remove 3 '(1 2 4 1 3 4 5) :test #'>) '(4 3 4 5))))
(define-test sacla-must-sequence.2847 (:tag :sacla)
 (assert-true
  (let* ((lst '(list of four elements))
         (lst2 (copy-seq lst))
         (lst3 (delete 'four lst)))
    (and (equal lst3 '(list of elements)) (not (equal lst lst2))))))
(define-test sacla-must-sequence.2848 (:tag :sacla)
 (assert-true (equal (remove-if #'oddp '(1 2 4 1 3 4 5)) '(2 4 4))))
(define-test sacla-must-sequence.2849 (:tag :sacla)
 (assert-true
  (equal (remove-if #'evenp '(1 2 4 1 3 4 5) :count 1 :from-end t)
         '(1 2 4 1 3 5))))
(define-test sacla-must-sequence.2850 (:tag :sacla)
 (assert-true
  (equal (remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9) :count 2 :from-end t)
         '(1 2 3 4 5 6 8))))
(define-test sacla-must-sequence.2851 (:tag :sacla)
 (assert-true (equal (delete 4 (list 1 2 4 1 3 4 5)) '(1 2 1 3 5))))
(define-test sacla-must-sequence.2852 (:tag :sacla)
 (assert-true (equal (delete 4 (list 1 2 4 1 3 4 5) :count 1) '(1 2 1 3 4 5))))
(define-test sacla-must-sequence.2853 (:tag :sacla)
 (assert-true
  (equal (delete 4 (list 1 2 4 1 3 4 5) :count 1 :from-end t) '(1 2 4 1 3 5))))
(define-test sacla-must-sequence.2854 (:tag :sacla)
 (assert-true (equal (delete 3 (list 1 2 4 1 3 4 5) :test #'>) '(4 3 4 5))))
(define-test sacla-must-sequence.2855 (:tag :sacla)
 (assert-true (equal (delete-if #'oddp (list 1 2 4 1 3 4 5)) '(2 4 4))))
(define-test sacla-must-sequence.2856 (:tag :sacla)
 (assert-true
  (equal (delete-if #'evenp (list 1 2 4 1 3 4 5) :count 1 :from-end t)
         '(1 2 4 1 3 5))))
(define-test sacla-must-sequence.2857 (:tag :sacla)
 (assert-true (equal (delete-if #'evenp (list 1 2 3 4 5 6)) '(1 3 5))))
(define-test sacla-must-sequence.2858 (:tag :sacla)
 (assert-true
  (let* ((list0 (list 0 1 2 3 4)) (list (remove 3 list0)))
    (and (not (eq list0 list))
         (equal list0 '(0 1 2 3 4))
         (equal list '(0 1 2 4))))))
(define-test sacla-must-sequence.2859 (:tag :sacla)
 (assert-true (equal (remove 'a (list 'a 'b 'c 'a 'b 'c)) '(b c b c))))
(define-test sacla-must-sequence.2860 (:tag :sacla)
 (assert-true (equal (remove 'b (list 'a 'b 'c 'a 'b 'c)) '(a c a c))))
(define-test sacla-must-sequence.2861 (:tag :sacla)
 (assert-true (equal (remove 'c (list 'a 'b 'c 'a 'b 'c)) '(a b a b))))
(define-test sacla-must-sequence.2862 (:tag :sacla)
 (assert-true (equal (remove 'a (list 'a 'a 'a)) 'nil)))
(define-test sacla-must-sequence.2863 (:tag :sacla)
 (assert-true (equal (remove 'z (list 'a 'b 'c)) '(a b c))))
(define-test sacla-must-sequence.2864 (:tag :sacla)
 (assert-true (equal (remove 'a 'nil) 'nil)))
(define-test sacla-must-sequence.2865 (:tag :sacla)
 (assert-true
  (equal (remove 'a (list 'a 'b 'c 'a 'b 'c) :count 0) '(a b c a b c))))
(define-test sacla-must-sequence.2866 (:tag :sacla)
 (assert-true
  (equal (remove 'a (list 'a 'b 'c 'a 'b 'c) :count 1) '(b c a b c))))
(define-test sacla-must-sequence.2867 (:tag :sacla)
 (assert-true
  (equal (remove 'a (list 'a 'b 'c 'a 'b 'c) :count 1 :from-end t)
         '(a b c b c))))
(define-test sacla-must-sequence.2868 (:tag :sacla)
 (assert-true (equal (remove 'a (list 'a 'b 'c 'a 'b 'c) :count 2) '(b c b c))))
(define-test sacla-must-sequence.2869 (:tag :sacla)
 (assert-true
  (equal (remove 'a (list 'a 'b 'c 'a 'b 'c) :count 2 :from-end t) '(b c b c))))
(define-test sacla-must-sequence.2870 (:tag :sacla)
 (assert-true (equal (remove 'a (list 'a 'b 'c 'a 'b 'c) :count 3) '(b c b c))))
(define-test sacla-must-sequence.2871 (:tag :sacla)
 (assert-true
  (equal (remove 'a (list 'a 'b 'c 'a 'b 'c) :count 3 :from-end t) '(b c b c))))
(define-test sacla-must-sequence.2872 (:tag :sacla)
 (assert-true (equal (remove 'a (list 'a 'b 'c 'a 'b 'c) :count 4) '(b c b c))))
(define-test sacla-must-sequence.2873 (:tag :sacla)
 (assert-true
  (equal (remove 'a (list 'a 'b 'c 'a 'b 'c) :count 4 :from-end t) '(b c b c))))
(define-test sacla-must-sequence.2874 (:tag :sacla)
 (assert-true
  (equal (remove 'a (list 'a 'b 'c 'a 'b 'c) :count -1) '(a b c a b c))))
(define-test sacla-must-sequence.2875 (:tag :sacla)
 (assert-true
  (equal (remove 'a (list 'a 'b 'c 'a 'b 'c) :count -10) '(a b c a b c))))
(define-test sacla-must-sequence.2876 (:tag :sacla)
 (assert-true
  (equal (remove 'a (list 'a 'b 'c 'a 'b 'c) :count -100) '(a b c a b c))))
(define-test sacla-must-sequence.2877 (:tag :sacla)
 (assert-true
  (equal (remove 'a (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c) :start 1)
         '(a b c b c b c b c))))
(define-test sacla-must-sequence.2878 (:tag :sacla)
 (assert-true
  (equal
   (remove 'a (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c) :start 1 :count 1)
   '(a b c b c a b c a b c))))
(define-test sacla-must-sequence.2879 (:tag :sacla)
 (assert-true
  (equal
   (remove 'a (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c) :start 1 :count 2)
   '(a b c b c b c a b c))))
(define-test sacla-must-sequence.2880 (:tag :sacla)
 (assert-true
  (equal
   (remove 'a
           (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
           :start 1
           :end nil
           :count 2)
   '(a b c b c b c a b c))))
(define-test sacla-must-sequence.2881 (:tag :sacla)
 (assert-true
  (equal (remove 'a (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c) :start 1 :end 8)
         '(a b c b c b c a b c))))
(define-test sacla-must-sequence.2882 (:tag :sacla)
 (assert-true
  (equal
   (remove 'a
           (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
           :start 1
           :end 8
           :count 1)
   '(a b c b c a b c a b c))))
(define-test sacla-must-sequence.2883 (:tag :sacla)
 (assert-true
  (equal
   (remove 'a
           (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
           :start 1
           :end 8
           :count 1
           :from-end t)
   '(a b c a b c b c a b c))))
(define-test sacla-must-sequence.2884 (:tag :sacla)
 (assert-true
  (equal
   (remove 'a
           (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
           :start 1
           :end 8
           :count 0
           :from-end t)
   '(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.2885 (:tag :sacla)
 (assert-true
  (equal
   (remove 'a
           (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
           :start 1
           :end 8
           :count -100
           :from-end t)
   '(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.2886 (:tag :sacla)
 (assert-true
  (equal (remove 'a (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c) :start 1 :end 1)
         '(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.2887 (:tag :sacla)
 (assert-true
  (equal (remove 'a (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c) :start 2 :end 2)
         '(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.2888 (:tag :sacla)
 (assert-true
  (equal
   (remove 'a (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c) :start 12 :end 12)
   '(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.2889 (:tag :sacla)
 (assert-true
  (equal (remove 'a (list '(a) '(b) '(c) '(a) '(b) '(c)) :key #'car)
         '((b) (c) (b) (c)))))
(define-test sacla-must-sequence.2890 (:tag :sacla)
 (assert-true
  (equal
   (remove 'a
           (list '(a . b) '(b . c) '(c . a) '(a . b) '(b . c) '(c . a))
           :key #'cdr)
   '((a . b) (b . c) (a . b) (b . c)))))
(define-test sacla-must-sequence.2891 (:tag :sacla)
 (assert-true
  (equal
   (remove "love" (list '("Love") '("LOve") '("LOVe") '("LOVE")) :key #'car)
   '(("Love") ("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.2892 (:tag :sacla)
 (assert-true
  (equal
   (remove "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :count -10)
   '(("Love") ("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.2893 (:tag :sacla)
 (assert-true
  (equal
   (remove "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test #'string-equal)
   'nil)))
(define-test sacla-must-sequence.2894 (:tag :sacla)
 (assert-true
  (equal
   (remove "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test-not (complement #'string-equal))
   'nil)))
(define-test sacla-must-sequence.2895 (:tag :sacla)
 (assert-true
  (equal
   (remove "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test #'string-equal
           :count 1)
   '(("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.2896 (:tag :sacla)
 (assert-true
  (equal
   (remove "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test-not (complement #'string-equal)
           :count 1)
   '(("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.2897 (:tag :sacla)
 (assert-true
  (equal
   (remove "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test #'string-equal
           :count 1
           :from-end t)
   '(("Love") ("LOve") ("LOVe")))))
(define-test sacla-must-sequence.2898 (:tag :sacla)
 (assert-true
  (equal
   (remove "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test-not (complement #'string-equal)
           :count 1
           :from-end t)
   '(("Love") ("LOve") ("LOVe")))))
(define-test sacla-must-sequence.2899 (:tag :sacla)
 (assert-true
  (equal
   (remove "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test #'string-equal
           :count 2
           :from-end t)
   '(("Love") ("LOve")))))
(define-test sacla-must-sequence.2900 (:tag :sacla)
 (assert-true
  (equal
   (remove "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test-not (complement #'string-equal)
           :count 2
           :from-end t)
   '(("Love") ("LOve")))))
(define-test sacla-must-sequence.2901 (:tag :sacla)
 (assert-true
  (equal
   (remove "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test #'string-equal
           :start 1
           :end 3)
   '(("Love") ("LOVE")))))
(define-test sacla-must-sequence.2902 (:tag :sacla)
 (assert-true
  (equal
   (remove "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test-not (complement #'string-equal)
           :start 1
           :end 3)
   '(("Love") ("LOVE")))))
(define-test sacla-must-sequence.2903 (:tag :sacla)
 (assert-true
  (equal
   (remove "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test #'string-equal
           :count 1
           :start 1
           :end 3)
   '(("Love") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.2904 (:tag :sacla)
 (assert-true
  (equal
   (remove "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test-not (complement #'string-equal)
           :count 1
           :start 1
           :end 3)
   '(("Love") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.2905 (:tag :sacla)
 (assert-true
  (equal
   (remove "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test #'string-equal
           :count 1
           :from-end t
           :start 1
           :end 3)
   '(("Love") ("LOve") ("LOVE")))))
(define-test sacla-must-sequence.2906 (:tag :sacla)
 (assert-true
  (equal
   (remove "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test-not (complement #'string-equal)
           :count 1
           :from-end t
           :start 1
           :end 3)
   '(("Love") ("LOve") ("LOVE")))))
(define-test sacla-must-sequence.2907 (:tag :sacla)
 (assert-true
  (equal
   (remove "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test #'string-equal
           :count 10
           :from-end t
           :start 1
           :end 3)
   '(("Love") ("LOVE")))))
(define-test sacla-must-sequence.2908 (:tag :sacla)
 (assert-true
  (equal
   (remove "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test-not (complement #'string-equal)
           :count 10
           :from-end t
           :start 1
           :end 3)
   '(("Love") ("LOVE")))))
(define-test sacla-must-sequence.2909 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 0 1 2 3 4)) (vector (remove 3 vector0)))
    (and (not (eq vector0 vector))
         (equalp vector0 #(0 1 2 3 4))
         (equalp vector #(0 1 2 4))))))
(define-test sacla-must-sequence.2910 (:tag :sacla)
 (assert-true (equalp (remove 'a (vector 'a 'b 'c 'a 'b 'c)) #(b c b c))))
(define-test sacla-must-sequence.2911 (:tag :sacla)
 (assert-true (equalp (remove 'b (vector 'a 'b 'c 'a 'b 'c)) #(a c a c))))
(define-test sacla-must-sequence.2912 (:tag :sacla)
 (assert-true (equalp (remove 'c (vector 'a 'b 'c 'a 'b 'c)) #(a b a b))))
(define-test sacla-must-sequence.2913 (:tag :sacla)
 (assert-true (equalp (remove 'a (vector 'a 'a 'a)) #())))
(define-test sacla-must-sequence.2914 (:tag :sacla)
 (assert-true (equalp (remove 'z (vector 'a 'b 'c)) #(a b c))))
(define-test sacla-must-sequence.2915 (:tag :sacla)
 (assert-true (equalp (remove 'a #()) #())))
(define-test sacla-must-sequence.2916 (:tag :sacla)
 (assert-true
  (equalp (remove 'a (vector 'a 'b 'c 'a 'b 'c) :count 0) #(a b c a b c))))
(define-test sacla-must-sequence.2917 (:tag :sacla)
 (assert-true
  (equalp (remove 'a (vector 'a 'b 'c 'a 'b 'c) :count 1) #(b c a b c))))
(define-test sacla-must-sequence.2918 (:tag :sacla)
 (assert-true
  (equalp (remove 'a (vector 'a 'b 'c 'a 'b 'c) :count 1 :from-end t)
          #(a b c b c))))
(define-test sacla-must-sequence.2919 (:tag :sacla)
 (assert-true
  (equalp (remove 'a (vector 'a 'b 'c 'a 'b 'c) :count 2) #(b c b c))))
(define-test sacla-must-sequence.2920 (:tag :sacla)
 (assert-true
  (equalp (remove 'a (vector 'a 'b 'c 'a 'b 'c) :count 2 :from-end t)
          #(b c b c))))
(define-test sacla-must-sequence.2921 (:tag :sacla)
 (assert-true
  (equalp (remove 'a (vector 'a 'b 'c 'a 'b 'c) :count 3) #(b c b c))))
(define-test sacla-must-sequence.2922 (:tag :sacla)
 (assert-true
  (equalp (remove 'a (vector 'a 'b 'c 'a 'b 'c) :count 3 :from-end t)
          #(b c b c))))
(define-test sacla-must-sequence.2923 (:tag :sacla)
 (assert-true
  (equalp (remove 'a (vector 'a 'b 'c 'a 'b 'c) :count 4) #(b c b c))))
(define-test sacla-must-sequence.2924 (:tag :sacla)
 (assert-true
  (equalp (remove 'a (vector 'a 'b 'c 'a 'b 'c) :count 4 :from-end t)
          #(b c b c))))
(define-test sacla-must-sequence.2925 (:tag :sacla)
 (assert-true
  (equalp (remove 'a (vector 'a 'b 'c 'a 'b 'c) :count -1) #(a b c a b c))))
(define-test sacla-must-sequence.2926 (:tag :sacla)
 (assert-true
  (equalp (remove 'a (vector 'a 'b 'c 'a 'b 'c) :count -10) #(a b c a b c))))
(define-test sacla-must-sequence.2927 (:tag :sacla)
 (assert-true
  (equalp (remove 'a (vector 'a 'b 'c 'a 'b 'c) :count -100) #(a b c a b c))))
(define-test sacla-must-sequence.2928 (:tag :sacla)
 (assert-true
  (equalp (remove 'a (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c) :start 1)
          #(a b c b c b c b c))))
(define-test sacla-must-sequence.2929 (:tag :sacla)
 (assert-true
  (equalp
   (remove 'a (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c) :start 1 :count 1)
   #(a b c b c a b c a b c))))
(define-test sacla-must-sequence.2930 (:tag :sacla)
 (assert-true
  (equalp
   (remove 'a (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c) :start 1 :count 2)
   #(a b c b c b c a b c))))
(define-test sacla-must-sequence.2931 (:tag :sacla)
 (assert-true
  (equalp
   (remove 'a
           (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
           :start 1
           :end nil
           :count 2)
   #(a b c b c b c a b c))))
(define-test sacla-must-sequence.2932 (:tag :sacla)
 (assert-true
  (equalp
   (remove 'a (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c) :start 1 :end 8)
   #(a b c b c b c a b c))))
(define-test sacla-must-sequence.2933 (:tag :sacla)
 (assert-true
  (equalp
   (remove 'a
           (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
           :start 1
           :end 8
           :count 1)
   #(a b c b c a b c a b c))))
(define-test sacla-must-sequence.2934 (:tag :sacla)
 (assert-true
  (equalp
   (remove 'a
           (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
           :start 1
           :end 8
           :count 1
           :from-end t)
   #(a b c a b c b c a b c))))
(define-test sacla-must-sequence.2935 (:tag :sacla)
 (assert-true
  (equalp
   (remove 'a
           (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
           :start 1
           :end 8
           :count 0
           :from-end t)
   #(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.2936 (:tag :sacla)
 (assert-true
  (equalp
   (remove 'a
           (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
           :start 1
           :end 8
           :count -100
           :from-end t)
   #(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.2937 (:tag :sacla)
 (assert-true
  (equalp
   (remove 'a (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c) :start 1 :end 1)
   #(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.2938 (:tag :sacla)
 (assert-true
  (equalp
   (remove 'a (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c) :start 2 :end 2)
   #(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.2939 (:tag :sacla)
 (assert-true
  (equalp
   (remove 'a (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c) :start 12 :end 12)
   #(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.2940 (:tag :sacla)
 (assert-true
  (equalp (remove 'a (vector '(a) '(b) '(c) '(a) '(b) '(c)) :key #'car)
          #((b) (c) (b) (c)))))
(define-test sacla-must-sequence.2941 (:tag :sacla)
 (assert-true
  (equalp
   (remove 'a
           (vector '(a . b) '(b . c) '(c . a) '(a . b) '(b . c) '(c . a))
           :key #'cdr)
   #((a . b) (b . c) (a . b) (b . c)))))
(define-test sacla-must-sequence.2942 (:tag :sacla)
 (assert-true
  (equalp
   (remove "love" (vector '("Love") '("LOve") '("LOVe") '("LOVE")) :key #'car)
   #(("Love") ("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.2943 (:tag :sacla)
 (assert-true
  (equalp
   (remove "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :count -10)
   #(("Love") ("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.2944 (:tag :sacla)
 (assert-true
  (equalp
   (remove "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test #'string-equal)
   #())))
(define-test sacla-must-sequence.2945 (:tag :sacla)
 (assert-true
  (equalp
   (remove "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test-not (complement #'string-equal))
   #())))
(define-test sacla-must-sequence.2946 (:tag :sacla)
 (assert-true
  (equalp
   (remove "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test #'string-equal
           :count 1)
   #(("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.2947 (:tag :sacla)
 (assert-true
  (equalp
   (remove "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test-not (complement #'string-equal)
           :count 1)
   #(("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.2948 (:tag :sacla)
 (assert-true
  (equalp
   (remove "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test #'string-equal
           :count 1
           :from-end t)
   #(("Love") ("LOve") ("LOVe")))))
(define-test sacla-must-sequence.2949 (:tag :sacla)
 (assert-true
  (equalp
   (remove "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test-not (complement #'string-equal)
           :count 1
           :from-end t)
   #(("Love") ("LOve") ("LOVe")))))
(define-test sacla-must-sequence.2950 (:tag :sacla)
 (assert-true
  (equalp
   (remove "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test #'string-equal
           :count 2
           :from-end t)
   #(("Love") ("LOve")))))
(define-test sacla-must-sequence.2951 (:tag :sacla)
 (assert-true
  (equalp
   (remove "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test-not (complement #'string-equal)
           :count 2
           :from-end t)
   #(("Love") ("LOve")))))
(define-test sacla-must-sequence.2952 (:tag :sacla)
 (assert-true
  (equalp
   (remove "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test #'string-equal
           :start 1
           :end 3)
   #(("Love") ("LOVE")))))
(define-test sacla-must-sequence.2953 (:tag :sacla)
 (assert-true
  (equalp
   (remove "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test-not (complement #'string-equal)
           :start 1
           :end 3)
   #(("Love") ("LOVE")))))
(define-test sacla-must-sequence.2954 (:tag :sacla)
 (assert-true
  (equalp
   (remove "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test #'string-equal
           :count 1
           :start 1
           :end 3)
   #(("Love") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.2955 (:tag :sacla)
 (assert-true
  (equalp
   (remove "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test-not (complement #'string-equal)
           :count 1
           :start 1
           :end 3)
   #(("Love") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.2956 (:tag :sacla)
 (assert-true
  (equalp
   (remove "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test #'string-equal
           :count 1
           :from-end t
           :start 1
           :end 3)
   #(("Love") ("LOve") ("LOVE")))))
(define-test sacla-must-sequence.2957 (:tag :sacla)
 (assert-true
  (equalp
   (remove "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test-not (complement #'string-equal)
           :count 1
           :from-end t
           :start 1
           :end 3)
   #(("Love") ("LOve") ("LOVE")))))
(define-test sacla-must-sequence.2958 (:tag :sacla)
 (assert-true
  (equalp
   (remove "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test #'string-equal
           :count 10
           :from-end t
           :start 1
           :end 3)
   #(("Love") ("LOVE")))))
(define-test sacla-must-sequence.2959 (:tag :sacla)
 (assert-true
  (equalp
   (remove "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test-not (complement #'string-equal)
           :count 10
           :from-end t
           :start 1
           :end 3)
   #(("Love") ("LOVE")))))
(define-test sacla-must-sequence.2960 (:tag :sacla)
 (assert-true (string= (remove #\a (copy-seq "abcabc")) "bcbc")))
(define-test sacla-must-sequence.2961 (:tag :sacla)
 (assert-true (string= (remove #\a (copy-seq "")) "")))
(define-test sacla-must-sequence.2962 (:tag :sacla)
 (assert-true (string= (remove #\a (copy-seq "xyz")) "xyz")))
(define-test sacla-must-sequence.2963 (:tag :sacla)
 (assert-true (string= (remove #\a (copy-seq "ABCABC")) "ABCABC")))
(define-test sacla-must-sequence.2964 (:tag :sacla)
 (assert-true
  (string= (remove #\a (copy-seq "ABCABC") :key #'char-downcase) "BCBC")))
(define-test sacla-must-sequence.2965 (:tag :sacla)
 (assert-true (string= (remove #\a (copy-seq "abcabc") :count 1) "bcabc")))
(define-test sacla-must-sequence.2966 (:tag :sacla)
 (assert-true
  (string= (remove #\a (copy-seq "abcabc") :count 1 :from-end t) "abcbc")))
(define-test sacla-must-sequence.2967 (:tag :sacla)
 (assert-true (string= (remove #\a (copy-seq "abcabc") :count 0) "abcabc")))
(define-test sacla-must-sequence.2968 (:tag :sacla)
 (assert-true (string= (remove #\a (copy-seq "abcabc") :count -10) "abcabc")))
(define-test sacla-must-sequence.2969 (:tag :sacla)
 (assert-true
  (let* ((str0 (copy-seq "abc")) (str (remove #\a str0)))
    (and (not (eq str0 str)) (string= str0 "abc") (string= str "bc")))))
(define-test sacla-must-sequence.2970 (:tag :sacla)
 (assert-true (string= (remove #\a (copy-seq "abcabc") :count 0) "abcabc")))
(define-test sacla-must-sequence.2971 (:tag :sacla)
 (assert-true (string= (remove #\a (copy-seq "abcabc")) "bcbc")))
(define-test sacla-must-sequence.2972 (:tag :sacla)
 (assert-true (string= (remove #\b (copy-seq "abcabc")) "acac")))
(define-test sacla-must-sequence.2973 (:tag :sacla)
 (assert-true (string= (remove #\c (copy-seq "abcabc")) "abab")))
(define-test sacla-must-sequence.2974 (:tag :sacla)
 (assert-true (string= (remove #\a (copy-seq "abcabc") :count 0) "abcabc")))
(define-test sacla-must-sequence.2975 (:tag :sacla)
 (assert-true (string= (remove #\a (copy-seq "abcabc") :count 1) "bcabc")))
(define-test sacla-must-sequence.2976 (:tag :sacla)
 (assert-true
  (string= (remove #\a (copy-seq "abcabc") :count 1 :from-end t) "abcbc")))
(define-test sacla-must-sequence.2977 (:tag :sacla)
 (assert-true (string= (remove #\a (copy-seq "abcabc") :count 2) "bcbc")))
(define-test sacla-must-sequence.2978 (:tag :sacla)
 (assert-true
  (string= (remove #\a (copy-seq "abcabc") :count 2 :from-end t) "bcbc")))
(define-test sacla-must-sequence.2979 (:tag :sacla)
 (assert-true (string= (remove #\a (copy-seq "abcabc") :count 3) "bcbc")))
(define-test sacla-must-sequence.2980 (:tag :sacla)
 (assert-true
  (string= (remove #\a (copy-seq "abcabc") :count 3 :from-end t) "bcbc")))
(define-test sacla-must-sequence.2981 (:tag :sacla)
 (assert-true (string= (remove #\a (copy-seq "abcabc") :count 4) "bcbc")))
(define-test sacla-must-sequence.2982 (:tag :sacla)
 (assert-true
  (string= (remove #\a (copy-seq "abcabc") :count 4 :from-end t) "bcbc")))
(define-test sacla-must-sequence.2983 (:tag :sacla)
 (assert-true (string= (remove #\a (copy-seq "abcabc") :count -1) "abcabc")))
(define-test sacla-must-sequence.2984 (:tag :sacla)
 (assert-true (string= (remove #\a (copy-seq "abcabc") :count -10) "abcabc")))
(define-test sacla-must-sequence.2985 (:tag :sacla)
 (assert-true (string= (remove #\a (copy-seq "abcabc") :count -100) "abcabc")))
(define-test sacla-must-sequence.2986 (:tag :sacla)
 (assert-true
  (string= (remove #\a (copy-seq "abcabcabcabc") :start 1) "abcbcbcbc")))
(define-test sacla-must-sequence.2987 (:tag :sacla)
 (assert-true
  (string= (remove #\a (copy-seq "abcabcabcabc") :start 1 :count 1)
           "abcbcabcabc")))
(define-test sacla-must-sequence.2988 (:tag :sacla)
 (assert-true
  (string= (remove #\a (copy-seq "abcabcabcabc") :start 1 :count 2)
           "abcbcbcabc")))
(define-test sacla-must-sequence.2989 (:tag :sacla)
 (assert-true
  (string= (remove #\a (copy-seq "abcabcabcabc") :start 1 :end nil :count 2)
           "abcbcbcabc")))
(define-test sacla-must-sequence.2990 (:tag :sacla)
 (assert-true
  (string= (remove #\a (copy-seq "abcabcabcabc") :start 1 :end 8)
           "abcbcbcabc")))
(define-test sacla-must-sequence.2991 (:tag :sacla)
 (assert-true
  (string= (remove #\a (copy-seq "abcabcabcabc") :start 1 :end 8 :count 1)
           "abcbcabcabc")))
(define-test sacla-must-sequence.2992 (:tag :sacla)
 (assert-true
  (string=
   (remove #\a (copy-seq "abcabcabcabc") :start 1 :end 8 :count 1 :from-end t)
   "abcabcbcabc")))
(define-test sacla-must-sequence.2993 (:tag :sacla)
 (assert-true
  (string=
   (remove #\a (copy-seq "abcabcabcabc") :start 1 :end 8 :count 0 :from-end t)
   "abcabcabcabc")))
(define-test sacla-must-sequence.2994 (:tag :sacla)
 (assert-true
  (string=
   (remove #\a
           (copy-seq "abcabcabcabc")
           :start 1
           :end 8
           :count -100
           :from-end t)
   "abcabcabcabc")))
(define-test sacla-must-sequence.2995 (:tag :sacla)
 (assert-true
  (string= (remove #\a (copy-seq "abcabcabcabc") :start 1 :end 1)
           "abcabcabcabc")))
(define-test sacla-must-sequence.2996 (:tag :sacla)
 (assert-true
  (string= (remove #\a (copy-seq "abcabcabcabc") :start 2 :end 2)
           "abcabcabcabc")))
(define-test sacla-must-sequence.2997 (:tag :sacla)
 (assert-true
  (string= (remove #\a (copy-seq "abcabcabcabc") :start 12 :end 12)
           "abcabcabcabc")))
(define-test sacla-must-sequence.2998 (:tag :sacla)
 (assert-true (equal (remove 0 #*0101) #*11)))
(define-test sacla-must-sequence.2999 (:tag :sacla)
 (assert-true (equal (remove 0 #*01010101 :count 1) #*1010101)))
(define-test sacla-must-sequence.3000 (:tag :sacla)
 (assert-true (equal (remove 0 #*01010101 :count 1 :from-end t) #*0101011)))
(define-test sacla-must-sequence.3001 (:tag :sacla)
 (assert-true (equal (remove 0 #*01010101 :start 1) #*01111)))
(define-test sacla-must-sequence.3002 (:tag :sacla)
 (assert-true (equal (remove 0 #*01010101 :start 1 :end nil) #*01111)))
(define-test sacla-must-sequence.3003 (:tag :sacla)
 (assert-true (equal (remove 0 #*01010101 :start 1 :end 6) #*011101)))
(define-test sacla-must-sequence.3004 (:tag :sacla)
 (assert-true (equal (remove 0 #*01010101 :start 1 :end 6 :count 1) #*0110101)))
(define-test sacla-must-sequence.3005 (:tag :sacla)
 (assert-true
  (equal (remove 0 #*01010101 :start 1 :end 6 :count 1 :from-end t) #*0101101)))
(define-test sacla-must-sequence.3006 (:tag :sacla)
 (assert-true
  (equal
   (remove 0
           #*01010101
           :start 1
           :end 6
           :count 1
           :from-end t
           :test #'(lambda (a b) (declare (ignore a)) (oddp b)))
   #*0101001)))
(define-test sacla-must-sequence.3007 (:tag :sacla)
 (assert-true
  (equal
   (remove 0
           #*01010101
           :start 1
           :end 6
           :count 1
           :from-end t
           :test-not #'(lambda (a b) (declare (ignore a)) (evenp b)))
   #*0101001)))
(define-test sacla-must-sequence.3008 (:tag :sacla)
 (assert-true
  (equal
   (remove 0
           #*01010101
           :start 1
           :end 6
           :count 1
           :from-end t
           :test #'(lambda (a b) (declare (ignore a)) (evenp b)))
   #*0101101)))
(define-test sacla-must-sequence.3009 (:tag :sacla)
 (assert-true
  (equal
   (remove 0
           #*01010101
           :start 1
           :end 6
           :count 1
           :from-end t
           :test-not #'(lambda (a b) (declare (ignore a)) (oddp b)))
   #*0101101)))
(define-test sacla-must-sequence.3010 (:tag :sacla)
 (assert-true
  (equal
   (remove 0
           #*01010101
           :start 1
           :end 6
           :count 1
           :from-end t
           :key #'(lambda (arg) (* arg 10))
           :test #'(lambda (a b) (declare (ignore a)) (> b 1)))
   #*0101001)))
(define-test sacla-must-sequence.3011 (:tag :sacla)
 (assert-true
  (let* ((list0 (list 0 1 2 3 4))
         (list (remove-if #'(lambda (arg) (eql arg 3)) list0)))
    (and (not (eq list0 list))
         (equal list0 '(0 1 2 3 4))
         (equal list '(0 1 2 4))))))
(define-test sacla-must-sequence.3012 (:tag :sacla)
 (assert-true
  (equal (remove-if #'(lambda (arg) (eql arg 'a)) (list 'a 'b 'c 'a 'b 'c))
         '(b c b c))))
(define-test sacla-must-sequence.3013 (:tag :sacla)
 (assert-true
  (equal (remove-if #'(lambda (arg) (eql arg 'b)) (list 'a 'b 'c 'a 'b 'c))
         '(a c a c))))
(define-test sacla-must-sequence.3014 (:tag :sacla)
 (assert-true
  (equal (remove-if #'(lambda (arg) (eql arg 'c)) (list 'a 'b 'c 'a 'b 'c))
         '(a b a b))))
(define-test sacla-must-sequence.3015 (:tag :sacla)
 (assert-true
  (equal (remove-if #'(lambda (arg) (eql arg 'a)) (list 'a 'a 'a)) 'nil)))
(define-test sacla-must-sequence.3016 (:tag :sacla)
 (assert-true
  (equal (remove-if #'(lambda (arg) (eql arg 'z)) (list 'a 'b 'c)) '(a b c))))
(define-test sacla-must-sequence.3017 (:tag :sacla)
 (assert-true (equal (remove-if #'(lambda (arg) (eql arg 'a)) 'nil) 'nil)))
(define-test sacla-must-sequence.3018 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (eql arg 'a)) (list 'a 'b 'c 'a 'b 'c) :count 0)
   '(a b c a b c))))
(define-test sacla-must-sequence.3019 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (eql arg 'a)) (list 'a 'b 'c 'a 'b 'c) :count 1)
   '(b c a b c))))
(define-test sacla-must-sequence.3020 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c)
              :count 1
              :from-end t)
   '(a b c b c))))
(define-test sacla-must-sequence.3021 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (eql arg 'a)) (list 'a 'b 'c 'a 'b 'c) :count 2)
   '(b c b c))))
(define-test sacla-must-sequence.3022 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c)
              :count 2
              :from-end t)
   '(b c b c))))
(define-test sacla-must-sequence.3023 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (eql arg 'a)) (list 'a 'b 'c 'a 'b 'c) :count 3)
   '(b c b c))))
(define-test sacla-must-sequence.3024 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c)
              :count 3
              :from-end t)
   '(b c b c))))
(define-test sacla-must-sequence.3025 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (eql arg 'a)) (list 'a 'b 'c 'a 'b 'c) :count 4)
   '(b c b c))))
(define-test sacla-must-sequence.3026 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c)
              :count 4
              :from-end t)
   '(b c b c))))
(define-test sacla-must-sequence.3027 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (eql arg 'a)) (list 'a 'b 'c 'a 'b 'c) :count -1)
   '(a b c a b c))))
(define-test sacla-must-sequence.3028 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c)
              :count -10)
   '(a b c a b c))))
(define-test sacla-must-sequence.3029 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c)
              :count -100)
   '(a b c a b c))))
(define-test sacla-must-sequence.3030 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1)
   '(a b c b c b c b c))))
(define-test sacla-must-sequence.3031 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :count 1)
   '(a b c b c a b c a b c))))
(define-test sacla-must-sequence.3032 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :count 2)
   '(a b c b c b c a b c))))
(define-test sacla-must-sequence.3033 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :end nil
              :count 2)
   '(a b c b c b c a b c))))
(define-test sacla-must-sequence.3034 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :end 8)
   '(a b c b c b c a b c))))
(define-test sacla-must-sequence.3035 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :end 8
              :count 1)
   '(a b c b c a b c a b c))))
(define-test sacla-must-sequence.3036 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :end 8
              :count 1
              :from-end t)
   '(a b c a b c b c a b c))))
(define-test sacla-must-sequence.3037 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :end 8
              :count 0
              :from-end t)
   '(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3038 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :end 8
              :count -100
              :from-end t)
   '(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3039 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :end 1)
   '(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3040 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 2
              :end 2)
   '(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3041 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 12
              :end 12)
   '(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3042 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (eql arg 'a))
              (list '(a) '(b) '(c) '(a) '(b) '(c))
              :key #'car)
   '((b) (c) (b) (c)))))
(define-test sacla-must-sequence.3043 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (eql arg 'a))
              (list '(a . b) '(b . c) '(c . a) '(a . b) '(b . c) '(c . a))
              :key #'cdr)
   '((a . b) (b . c) (a . b) (b . c)))))
(define-test sacla-must-sequence.3044 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (eql arg "love"))
              (list '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car)
   '(("Love") ("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3045 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (eql arg "love"))
              (list '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count -10)
   '(("Love") ("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3046 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (string-equal arg "love"))
              (list '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car)
   'nil)))
(define-test sacla-must-sequence.3047 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (string-equal arg "love"))
              (list '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count 1)
   '(("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3048 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (string-equal arg "love"))
              (list '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count 1
              :from-end t)
   '(("Love") ("LOve") ("LOVe")))))
(define-test sacla-must-sequence.3049 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (string-equal arg "love"))
              (list '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count 2
              :from-end t)
   '(("Love") ("LOve")))))
(define-test sacla-must-sequence.3050 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (string-equal arg "love"))
              (list '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :start 1
              :end 3)
   '(("Love") ("LOVE")))))
(define-test sacla-must-sequence.3051 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (string-equal arg "love"))
              (list '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count 1
              :start 1
              :end 3)
   '(("Love") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3052 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (string-equal arg "love"))
              (list '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count 1
              :from-end t
              :start 1
              :end 3)
   '(("Love") ("LOve") ("LOVE")))))
(define-test sacla-must-sequence.3053 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'(lambda (arg) (string-equal arg "love"))
              (list '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count 10
              :from-end t
              :start 1
              :end 3)
   '(("Love") ("LOVE")))))
(define-test sacla-must-sequence.3054 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 0 1 2 3 4))
         (vector (remove-if #'(lambda (arg) (eql arg 3)) vector0)))
    (and (not (eq vector0 vector))
         (equalp vector0 #(0 1 2 3 4))
         (equalp vector #(0 1 2 4))))))
(define-test sacla-must-sequence.3055 (:tag :sacla)
 (assert-true
  (equalp (remove-if #'(lambda (arg) (eql arg 'a)) (vector 'a 'b 'c 'a 'b 'c))
          #(b c b c))))
(define-test sacla-must-sequence.3056 (:tag :sacla)
 (assert-true
  (equalp (remove-if #'(lambda (arg) (eql arg 'b)) (vector 'a 'b 'c 'a 'b 'c))
          #(a c a c))))
(define-test sacla-must-sequence.3057 (:tag :sacla)
 (assert-true
  (equalp (remove-if #'(lambda (arg) (eql arg 'c)) (vector 'a 'b 'c 'a 'b 'c))
          #(a b a b))))
(define-test sacla-must-sequence.3058 (:tag :sacla)
 (assert-true
  (equalp (remove-if #'(lambda (arg) (eql arg 'a)) (vector 'a 'a 'a)) #())))
(define-test sacla-must-sequence.3059 (:tag :sacla)
 (assert-true
  (equalp (remove-if #'(lambda (arg) (eql arg 'z)) (vector 'a 'b 'c))
          #(a b c))))
(define-test sacla-must-sequence.3060 (:tag :sacla)
 (assert-true (equalp (remove-if #'(lambda (arg) (eql arg 'a)) #()) #())))
(define-test sacla-must-sequence.3061 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c)
              :count 0)
   #(a b c a b c))))
(define-test sacla-must-sequence.3062 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c)
              :count 1)
   #(b c a b c))))
(define-test sacla-must-sequence.3063 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c)
              :count 1
              :from-end t)
   #(a b c b c))))
(define-test sacla-must-sequence.3064 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c)
              :count 2)
   #(b c b c))))
(define-test sacla-must-sequence.3065 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c)
              :count 2
              :from-end t)
   #(b c b c))))
(define-test sacla-must-sequence.3066 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c)
              :count 3)
   #(b c b c))))
(define-test sacla-must-sequence.3067 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c)
              :count 3
              :from-end t)
   #(b c b c))))
(define-test sacla-must-sequence.3068 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c)
              :count 4)
   #(b c b c))))
(define-test sacla-must-sequence.3069 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c)
              :count 4
              :from-end t)
   #(b c b c))))
(define-test sacla-must-sequence.3070 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c)
              :count -1)
   #(a b c a b c))))
(define-test sacla-must-sequence.3071 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c)
              :count -10)
   #(a b c a b c))))
(define-test sacla-must-sequence.3072 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c)
              :count -100)
   #(a b c a b c))))
(define-test sacla-must-sequence.3073 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1)
   #(a b c b c b c b c))))
(define-test sacla-must-sequence.3074 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :count 1)
   #(a b c b c a b c a b c))))
(define-test sacla-must-sequence.3075 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :count 2)
   #(a b c b c b c a b c))))
(define-test sacla-must-sequence.3076 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :end nil
              :count 2)
   #(a b c b c b c a b c))))
(define-test sacla-must-sequence.3077 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :end 8)
   #(a b c b c b c a b c))))
(define-test sacla-must-sequence.3078 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :end 8
              :count 1)
   #(a b c b c a b c a b c))))
(define-test sacla-must-sequence.3079 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :end 8
              :count 1
              :from-end t)
   #(a b c a b c b c a b c))))
(define-test sacla-must-sequence.3080 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :end 8
              :count 0
              :from-end t)
   #(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3081 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :end 8
              :count -100
              :from-end t)
   #(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3082 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :end 1)
   #(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3083 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 2
              :end 2)
   #(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3084 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 12
              :end 12)
   #(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3085 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (eql arg 'a))
              (vector '(a) '(b) '(c) '(a) '(b) '(c))
              :key #'car)
   #((b) (c) (b) (c)))))
(define-test sacla-must-sequence.3086 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (eql arg 'a))
              (vector '(a . b) '(b . c) '(c . a) '(a . b) '(b . c) '(c . a))
              :key #'cdr)
   #((a . b) (b . c) (a . b) (b . c)))))
(define-test sacla-must-sequence.3087 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (eql arg "love"))
              (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car)
   #(("Love") ("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3088 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (eql arg "love"))
              (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count -10)
   #(("Love") ("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3089 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (string-equal arg "love"))
              (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car)
   #())))
(define-test sacla-must-sequence.3090 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (string-equal arg "love"))
              (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car)
   #())))
(define-test sacla-must-sequence.3091 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (string-equal arg "love"))
              (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count 1)
   #(("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3092 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (string-equal arg "love"))
              (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count 1)
   #(("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3093 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (string-equal arg "love"))
              (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count 1
              :from-end t)
   #(("Love") ("LOve") ("LOVe")))))
(define-test sacla-must-sequence.3094 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (string-equal arg "love"))
              (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count 1
              :from-end t)
   #(("Love") ("LOve") ("LOVe")))))
(define-test sacla-must-sequence.3095 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (string-equal arg "love"))
              (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count 2
              :from-end t)
   #(("Love") ("LOve")))))
(define-test sacla-must-sequence.3096 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (string-equal arg "love"))
              (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count 2
              :from-end t)
   #(("Love") ("LOve")))))
(define-test sacla-must-sequence.3097 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (string-equal arg "love"))
              (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :start 1
              :end 3)
   #(("Love") ("LOVE")))))
(define-test sacla-must-sequence.3098 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (string-equal arg "love"))
              (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count 1
              :start 1
              :end 3)
   #(("Love") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3099 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (string-equal arg "love"))
              (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count 1
              :from-end t
              :start 1
              :end 3)
   #(("Love") ("LOve") ("LOVE")))))
(define-test sacla-must-sequence.3100 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if #'(lambda (arg) (string-equal arg "love"))
              (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count 10
              :from-end t
              :start 1
              :end 3)
   #(("Love") ("LOVE")))))
(define-test sacla-must-sequence.3101 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (string-equal arg #\a)) (copy-seq "abcabc"))
   "bcbc")))
(define-test sacla-must-sequence.3102 (:tag :sacla)
 (assert-true
  (string= (remove-if #'(lambda (arg) (string-equal arg #\a)) (copy-seq ""))
           "")))
(define-test sacla-must-sequence.3103 (:tag :sacla)
 (assert-true
  (string= (remove-if #'(lambda (arg) (string-equal arg #\a)) (copy-seq "xyz"))
           "xyz")))
(define-test sacla-must-sequence.3104 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "ABCABC")
              :key #'char-downcase)
   "BCBC")))
(define-test sacla-must-sequence.3105 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count 1)
   "bcabc")))
(define-test sacla-must-sequence.3106 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count 1
              :from-end t)
   "abcbc")))
(define-test sacla-must-sequence.3107 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count 0)
   "abcabc")))
(define-test sacla-must-sequence.3108 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count -10)
   "abcabc")))
(define-test sacla-must-sequence.3109 (:tag :sacla)
 (assert-true
  (let* ((str0 (copy-seq "abc"))
         (str (remove-if #'(lambda (arg) (string-equal arg #\a)) str0)))
    (and (not (eq str0 str)) (string= str0 "abc") (string= str "bc")))))
(define-test sacla-must-sequence.3110 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count 0)
   "abcabc")))
(define-test sacla-must-sequence.3111 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (string-equal arg #\a)) (copy-seq "abcabc"))
   "bcbc")))
(define-test sacla-must-sequence.3112 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (string-equal arg #\b)) (copy-seq "abcabc"))
   "acac")))
(define-test sacla-must-sequence.3113 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (string-equal arg #\c)) (copy-seq "abcabc"))
   "abab")))
(define-test sacla-must-sequence.3114 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count 0)
   "abcabc")))
(define-test sacla-must-sequence.3115 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count 1)
   "bcabc")))
(define-test sacla-must-sequence.3116 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count 1
              :from-end t)
   "abcbc")))
(define-test sacla-must-sequence.3117 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count 2)
   "bcbc")))
(define-test sacla-must-sequence.3118 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count 2
              :from-end t)
   "bcbc")))
(define-test sacla-must-sequence.3119 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count 3)
   "bcbc")))
(define-test sacla-must-sequence.3120 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count 3
              :from-end t)
   "bcbc")))
(define-test sacla-must-sequence.3121 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count 4)
   "bcbc")))
(define-test sacla-must-sequence.3122 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count 4
              :from-end t)
   "bcbc")))
(define-test sacla-must-sequence.3123 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count -1)
   "abcabc")))
(define-test sacla-must-sequence.3124 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count -10)
   "abcabc")))
(define-test sacla-must-sequence.3125 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count -100)
   "abcabc")))
(define-test sacla-must-sequence.3126 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabcabcabc")
              :start 1)
   "abcbcbcbc")))
(define-test sacla-must-sequence.3127 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabcabcabc")
              :start 1
              :count 1)
   "abcbcabcabc")))
(define-test sacla-must-sequence.3128 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (eql arg #\a))
              (copy-seq "abcabcabcabc")
              :start 1
              :count 2)
   "abcbcbcabc")))
(define-test sacla-must-sequence.3129 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (eql arg #\a))
              (copy-seq "abcabcabcabc")
              :start 1
              :end nil
              :count 2)
   "abcbcbcabc")))
(define-test sacla-must-sequence.3130 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (eql arg #\a))
              (copy-seq "abcabcabcabc")
              :start 1
              :end 8)
   "abcbcbcabc")))
(define-test sacla-must-sequence.3131 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (eql arg #\a))
              (copy-seq "abcabcabcabc")
              :start 1
              :end 8
              :count 1)
   "abcbcabcabc")))
(define-test sacla-must-sequence.3132 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (eql arg #\a))
              (copy-seq "abcabcabcabc")
              :start 1
              :end 8
              :count 1
              :from-end t)
   "abcabcbcabc")))
(define-test sacla-must-sequence.3133 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (eql arg #\a))
              (copy-seq "abcabcabcabc")
              :start 1
              :end 8
              :count 0
              :from-end t)
   "abcabcabcabc")))
(define-test sacla-must-sequence.3134 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (eql arg #\a))
              (copy-seq "abcabcabcabc")
              :start 1
              :end 8
              :count -100
              :from-end t)
   "abcabcabcabc")))
(define-test sacla-must-sequence.3135 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (eql arg #\a))
              (copy-seq "abcabcabcabc")
              :start 1
              :end 1)
   "abcabcabcabc")))
(define-test sacla-must-sequence.3136 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (eql arg #\a))
              (copy-seq "abcabcabcabc")
              :start 2
              :end 2)
   "abcabcabcabc")))
(define-test sacla-must-sequence.3137 (:tag :sacla)
 (assert-true
  (string=
   (remove-if #'(lambda (arg) (eql arg #\a))
              (copy-seq "abcabcabcabc")
              :start 12
              :end 12)
   "abcabcabcabc")))
(define-test sacla-must-sequence.3138 (:tag :sacla)
 (assert-true (equal (remove-if #'zerop #*0101) #*11)))
(define-test sacla-must-sequence.3139 (:tag :sacla)
 (assert-true (equal (remove-if #'zerop #*01010101 :count 1) #*1010101)))
(define-test sacla-must-sequence.3140 (:tag :sacla)
 (assert-true
  (equal (remove-if #'zerop #*01010101 :count 1 :from-end t) #*0101011)))
(define-test sacla-must-sequence.3141 (:tag :sacla)
 (assert-true (equal (remove-if #'zerop #*01010101 :start 1) #*01111)))
(define-test sacla-must-sequence.3142 (:tag :sacla)
 (assert-true (equal (remove-if #'zerop #*01010101 :start 1 :end nil) #*01111)))
(define-test sacla-must-sequence.3143 (:tag :sacla)
 (assert-true (equal (remove-if #'zerop #*01010101 :start 1 :end 6) #*011101)))
(define-test sacla-must-sequence.3144 (:tag :sacla)
 (assert-true
  (equal (remove-if #'zerop #*01010101 :start 1 :end 6 :count 1) #*0110101)))
(define-test sacla-must-sequence.3145 (:tag :sacla)
 (assert-true
  (equal (remove-if #'zerop #*01010101 :start 1 :end 6 :count 1 :from-end t)
         #*0101101)))
(define-test sacla-must-sequence.3146 (:tag :sacla)
 (assert-true
  (equal (remove-if #'oddp #*01010101 :start 1 :end 6 :count 1 :from-end t)
         #*0101001)))
(define-test sacla-must-sequence.3147 (:tag :sacla)
 (assert-true
  (equal (remove-if #'evenp #*01010101 :start 1 :end 6 :count 1 :from-end t)
         #*0101101)))
(define-test sacla-must-sequence.3148 (:tag :sacla)
 (assert-true
  (equal
   (remove-if #'plusp
              #*01010101
              :start 1
              :end 6
              :count 1
              :from-end t
              :key #'(lambda (arg) (* arg 10)))
   #*0101001)))
(define-test sacla-must-sequence.3149 (:tag :sacla)
 (assert-true
  (let* ((list0 (list 0 1 2 3 4))
         (list (remove-if-not #'(lambda (arg) (not (eql arg 3))) list0)))
    (and (not (eq list0 list))
         (equal list0 '(0 1 2 3 4))
         (equal list '(0 1 2 4))))))
(define-test sacla-must-sequence.3150 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (eql arg 'a))) (list 'a 'b 'c 'a 'b 'c))
   '(b c b c))))
(define-test sacla-must-sequence.3151 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (eql arg 'b))) (list 'a 'b 'c 'a 'b 'c))
   '(a c a c))))
(define-test sacla-must-sequence.3152 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (eql arg 'c))) (list 'a 'b 'c 'a 'b 'c))
   '(a b a b))))
(define-test sacla-must-sequence.3153 (:tag :sacla)
 (assert-true
  (equal (remove-if-not #'(lambda (arg) (not (eql arg 'a))) (list 'a 'a 'a))
         'nil)))
(define-test sacla-must-sequence.3154 (:tag :sacla)
 (assert-true
  (equal (remove-if-not #'(lambda (arg) (not (eql arg 'z))) (list 'a 'b 'c))
         '(a b c))))
(define-test sacla-must-sequence.3155 (:tag :sacla)
 (assert-true
  (equal (remove-if-not #'(lambda (arg) (not (eql arg 'a))) 'nil) 'nil)))
(define-test sacla-must-sequence.3156 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c)
                  :count 0)
   '(a b c a b c))))
(define-test sacla-must-sequence.3157 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c)
                  :count 1)
   '(b c a b c))))
(define-test sacla-must-sequence.3158 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c)
                  :count 1
                  :from-end t)
   '(a b c b c))))
(define-test sacla-must-sequence.3159 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c)
                  :count 2)
   '(b c b c))))
(define-test sacla-must-sequence.3160 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c)
                  :count 2
                  :from-end t)
   '(b c b c))))
(define-test sacla-must-sequence.3161 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c)
                  :count 3)
   '(b c b c))))
(define-test sacla-must-sequence.3162 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c)
                  :count 3
                  :from-end t)
   '(b c b c))))
(define-test sacla-must-sequence.3163 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c)
                  :count 4)
   '(b c b c))))
(define-test sacla-must-sequence.3164 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c)
                  :count 4
                  :from-end t)
   '(b c b c))))
(define-test sacla-must-sequence.3165 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c)
                  :count -1)
   '(a b c a b c))))
(define-test sacla-must-sequence.3166 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c)
                  :count -10)
   '(a b c a b c))))
(define-test sacla-must-sequence.3167 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c)
                  :count -100)
   '(a b c a b c))))
(define-test sacla-must-sequence.3168 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1)
   '(a b c b c b c b c))))
(define-test sacla-must-sequence.3169 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :count 1)
   '(a b c b c a b c a b c))))
(define-test sacla-must-sequence.3170 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :count 2)
   '(a b c b c b c a b c))))
(define-test sacla-must-sequence.3171 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :end nil
                  :count 2)
   '(a b c b c b c a b c))))
(define-test sacla-must-sequence.3172 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :end 8)
   '(a b c b c b c a b c))))
(define-test sacla-must-sequence.3173 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :end 8
                  :count 1)
   '(a b c b c a b c a b c))))
(define-test sacla-must-sequence.3174 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :end 8
                  :count 1
                  :from-end t)
   '(a b c a b c b c a b c))))
(define-test sacla-must-sequence.3175 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :end 8
                  :count 0
                  :from-end t)
   '(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3176 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :end 8
                  :count -100
                  :from-end t)
   '(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3177 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :end 1)
   '(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3178 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 2
                  :end 2)
   '(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3179 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 12
                  :end 12)
   '(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3180 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list '(a) '(b) '(c) '(a) '(b) '(c))
                  :key #'car)
   '((b) (c) (b) (c)))))
(define-test sacla-must-sequence.3181 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list '(a . b) '(b . c) '(c . a) '(a . b) '(b . c) '(c . a))
                  :key #'cdr)
   '((a . b) (b . c) (a . b) (b . c)))))
(define-test sacla-must-sequence.3182 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (eql arg "love")))
                  (list '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car)
   '(("Love") ("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3183 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (eql arg "love")))
                  (list '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count -10)
   '(("Love") ("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3184 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (list '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car)
   'nil)))
(define-test sacla-must-sequence.3185 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (list '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count 1)
   '(("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3186 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (list '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count 1
                  :from-end t)
   '(("Love") ("LOve") ("LOVe")))))
(define-test sacla-must-sequence.3187 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (list '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count 2
                  :from-end t)
   '(("Love") ("LOve")))))
(define-test sacla-must-sequence.3188 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (list '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :start 1
                  :end 3)
   '(("Love") ("LOVE")))))
(define-test sacla-must-sequence.3189 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (list '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count 1
                  :start 1
                  :end 3)
   '(("Love") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3190 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (list '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count 1
                  :from-end t
                  :start 1
                  :end 3)
   '(("Love") ("LOve") ("LOVE")))))
(define-test sacla-must-sequence.3191 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (list '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count 10
                  :from-end t
                  :start 1
                  :end 3)
   '(("Love") ("LOVE")))))
(define-test sacla-must-sequence.3192 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 0 1 2 3 4))
         (vector (remove-if-not #'(lambda (arg) (not (eql arg 3))) vector0)))
    (and (not (eq vector0 vector))
         (equalp vector0 #(0 1 2 3 4))
         (equalp vector #(0 1 2 4))))))
(define-test sacla-must-sequence.3193 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c))
   #(b c b c))))
(define-test sacla-must-sequence.3194 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (eql arg 'b)))
                  (vector 'a 'b 'c 'a 'b 'c))
   #(a c a c))))
(define-test sacla-must-sequence.3195 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (eql arg 'c)))
                  (vector 'a 'b 'c 'a 'b 'c))
   #(a b a b))))
(define-test sacla-must-sequence.3196 (:tag :sacla)
 (assert-true
  (equalp (remove-if-not #'(lambda (arg) (not (eql arg 'a))) (vector 'a 'a 'a))
          #())))
(define-test sacla-must-sequence.3197 (:tag :sacla)
 (assert-true
  (equalp (remove-if-not #'(lambda (arg) (not (eql arg 'z))) (vector 'a 'b 'c))
          #(a b c))))
(define-test sacla-must-sequence.3198 (:tag :sacla)
 (assert-true
  (equalp (remove-if-not #'(lambda (arg) (not (eql arg 'a))) #()) #())))
(define-test sacla-must-sequence.3199 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c)
                  :count 0)
   #(a b c a b c))))
(define-test sacla-must-sequence.3200 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c)
                  :count 1)
   #(b c a b c))))
(define-test sacla-must-sequence.3201 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c)
                  :count 1
                  :from-end t)
   #(a b c b c))))
(define-test sacla-must-sequence.3202 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c)
                  :count 2)
   #(b c b c))))
(define-test sacla-must-sequence.3203 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c)
                  :count 2
                  :from-end t)
   #(b c b c))))
(define-test sacla-must-sequence.3204 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c)
                  :count 3)
   #(b c b c))))
(define-test sacla-must-sequence.3205 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c)
                  :count 3
                  :from-end t)
   #(b c b c))))
(define-test sacla-must-sequence.3206 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c)
                  :count 4)
   #(b c b c))))
(define-test sacla-must-sequence.3207 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c)
                  :count 4
                  :from-end t)
   #(b c b c))))
(define-test sacla-must-sequence.3208 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c)
                  :count -1)
   #(a b c a b c))))
(define-test sacla-must-sequence.3209 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c)
                  :count -10)
   #(a b c a b c))))
(define-test sacla-must-sequence.3210 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c)
                  :count -100)
   #(a b c a b c))))
(define-test sacla-must-sequence.3211 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1)
   #(a b c b c b c b c))))
(define-test sacla-must-sequence.3212 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :count 1)
   #(a b c b c a b c a b c))))
(define-test sacla-must-sequence.3213 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :count 2)
   #(a b c b c b c a b c))))
(define-test sacla-must-sequence.3214 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :end nil
                  :count 2)
   #(a b c b c b c a b c))))
(define-test sacla-must-sequence.3215 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :end 8)
   #(a b c b c b c a b c))))
(define-test sacla-must-sequence.3216 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :end 8
                  :count 1)
   #(a b c b c a b c a b c))))
(define-test sacla-must-sequence.3217 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :end 8
                  :count 1
                  :from-end t)
   #(a b c a b c b c a b c))))
(define-test sacla-must-sequence.3218 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :end 8
                  :count 0
                  :from-end t)
   #(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3219 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :end 8
                  :count -100
                  :from-end t)
   #(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3220 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :end 1)
   #(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3221 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 2
                  :end 2)
   #(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3222 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 12
                  :end 12)
   #(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3223 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector '(a) '(b) '(c) '(a) '(b) '(c))
                  :key #'car)
   #((b) (c) (b) (c)))))
(define-test sacla-must-sequence.3224 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector '(a . b)
                          '(b . c)
                          '(c . a)
                          '(a . b)
                          '(b . c)
                          '(c . a))
                  :key #'cdr)
   #((a . b) (b . c) (a . b) (b . c)))))
(define-test sacla-must-sequence.3225 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (eql arg "love")))
                  (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car)
   #(("Love") ("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3226 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (eql arg "love")))
                  (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count -10)
   #(("Love") ("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3227 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car)
   #())))
(define-test sacla-must-sequence.3228 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car)
   #())))
(define-test sacla-must-sequence.3229 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count 1)
   #(("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3230 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count 1)
   #(("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3231 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count 1
                  :from-end t)
   #(("Love") ("LOve") ("LOVe")))))
(define-test sacla-must-sequence.3232 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count 1
                  :from-end t)
   #(("Love") ("LOve") ("LOVe")))))
(define-test sacla-must-sequence.3233 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count 2
                  :from-end t)
   #(("Love") ("LOve")))))
(define-test sacla-must-sequence.3234 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count 2
                  :from-end t)
   #(("Love") ("LOve")))))
(define-test sacla-must-sequence.3235 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :start 1
                  :end 3)
   #(("Love") ("LOVE")))))
(define-test sacla-must-sequence.3236 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count 1
                  :start 1
                  :end 3)
   #(("Love") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3237 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count 1
                  :from-end t
                  :start 1
                  :end 3)
   #(("Love") ("LOve") ("LOVE")))))
(define-test sacla-must-sequence.3238 (:tag :sacla)
 (assert-true
  (equalp
   (remove-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count 10
                  :from-end t
                  :start 1
                  :end 3)
   #(("Love") ("LOVE")))))
(define-test sacla-must-sequence.3239 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc"))
   "bcbc")))
(define-test sacla-must-sequence.3240 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (string-equal arg #\a))) (copy-seq ""))
   "")))
(define-test sacla-must-sequence.3241 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "xyz"))
   "xyz")))
(define-test sacla-must-sequence.3242 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "ABCABC")
                  :key #'char-downcase)
   "BCBC")))
(define-test sacla-must-sequence.3243 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count 1)
   "bcabc")))
(define-test sacla-must-sequence.3244 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count 1
                  :from-end t)
   "abcbc")))
(define-test sacla-must-sequence.3245 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count 0)
   "abcabc")))
(define-test sacla-must-sequence.3246 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count -10)
   "abcabc")))
(define-test sacla-must-sequence.3247 (:tag :sacla)
 (assert-true
  (let* ((str0 (copy-seq "abc"))
         (str
          (remove-if-not #'(lambda (arg) (not (string-equal arg #\a))) str0)))
    (and (not (eq str0 str)) (string= str0 "abc") (string= str "bc")))))
(define-test sacla-must-sequence.3248 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count 0)
   "abcabc")))
(define-test sacla-must-sequence.3249 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc"))
   "bcbc")))
(define-test sacla-must-sequence.3250 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (string-equal arg #\b)))
                  (copy-seq "abcabc"))
   "acac")))
(define-test sacla-must-sequence.3251 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (string-equal arg #\c)))
                  (copy-seq "abcabc"))
   "abab")))
(define-test sacla-must-sequence.3252 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count 0)
   "abcabc")))
(define-test sacla-must-sequence.3253 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count 1)
   "bcabc")))
(define-test sacla-must-sequence.3254 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count 1
                  :from-end t)
   "abcbc")))
(define-test sacla-must-sequence.3255 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count 2)
   "bcbc")))
(define-test sacla-must-sequence.3256 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count 2
                  :from-end t)
   "bcbc")))
(define-test sacla-must-sequence.3257 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count 3)
   "bcbc")))
(define-test sacla-must-sequence.3258 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count 3
                  :from-end t)
   "bcbc")))
(define-test sacla-must-sequence.3259 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count 4)
   "bcbc")))
(define-test sacla-must-sequence.3260 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count 4
                  :from-end t)
   "bcbc")))
(define-test sacla-must-sequence.3261 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count -1)
   "abcabc")))
(define-test sacla-must-sequence.3262 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count -10)
   "abcabc")))
(define-test sacla-must-sequence.3263 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count -100)
   "abcabc")))
(define-test sacla-must-sequence.3264 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabcabcabc")
                  :start 1)
   "abcbcbcbc")))
(define-test sacla-must-sequence.3265 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabcabcabc")
                  :start 1
                  :count 1)
   "abcbcabcabc")))
(define-test sacla-must-sequence.3266 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (eql arg #\a)))
                  (copy-seq "abcabcabcabc")
                  :start 1
                  :count 2)
   "abcbcbcabc")))
(define-test sacla-must-sequence.3267 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (eql arg #\a)))
                  (copy-seq "abcabcabcabc")
                  :start 1
                  :end nil
                  :count 2)
   "abcbcbcabc")))
(define-test sacla-must-sequence.3268 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (eql arg #\a)))
                  (copy-seq "abcabcabcabc")
                  :start 1
                  :end 8)
   "abcbcbcabc")))
(define-test sacla-must-sequence.3269 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (eql arg #\a)))
                  (copy-seq "abcabcabcabc")
                  :start 1
                  :end 8
                  :count 1)
   "abcbcabcabc")))
(define-test sacla-must-sequence.3270 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (eql arg #\a)))
                  (copy-seq "abcabcabcabc")
                  :start 1
                  :end 8
                  :count 1
                  :from-end t)
   "abcabcbcabc")))
(define-test sacla-must-sequence.3271 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (eql arg #\a)))
                  (copy-seq "abcabcabcabc")
                  :start 1
                  :end 8
                  :count 0
                  :from-end t)
   "abcabcabcabc")))
(define-test sacla-must-sequence.3272 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (eql arg #\a)))
                  (copy-seq "abcabcabcabc")
                  :start 1
                  :end 8
                  :count -100
                  :from-end t)
   "abcabcabcabc")))
(define-test sacla-must-sequence.3273 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (eql arg #\a)))
                  (copy-seq "abcabcabcabc")
                  :start 1
                  :end 1)
   "abcabcabcabc")))
(define-test sacla-must-sequence.3274 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (eql arg #\a)))
                  (copy-seq "abcabcabcabc")
                  :start 2
                  :end 2)
   "abcabcabcabc")))
(define-test sacla-must-sequence.3275 (:tag :sacla)
 (assert-true
  (string=
   (remove-if-not #'(lambda (arg) (not (eql arg #\a)))
                  (copy-seq "abcabcabcabc")
                  :start 12
                  :end 12)
   "abcabcabcabc")))
(define-test sacla-must-sequence.3276 (:tag :sacla)
 (assert-true (equal (remove-if-not (complement #'zerop) #*0101) #*11)))
(define-test sacla-must-sequence.3277 (:tag :sacla)
 (assert-true
  (equal (remove-if-not (complement #'zerop) #*01010101 :count 1) #*1010101)))
(define-test sacla-must-sequence.3278 (:tag :sacla)
 (assert-true
  (equal (remove-if-not (complement #'zerop) #*01010101 :count 1 :from-end t)
         #*0101011)))
(define-test sacla-must-sequence.3279 (:tag :sacla)
 (assert-true
  (equal (remove-if-not (complement #'zerop) #*01010101 :start 1) #*01111)))
(define-test sacla-must-sequence.3280 (:tag :sacla)
 (assert-true
  (equal (remove-if-not (complement #'zerop) #*01010101 :start 1 :end nil)
         #*01111)))
(define-test sacla-must-sequence.3281 (:tag :sacla)
 (assert-true
  (equal (remove-if-not (complement #'zerop) #*01010101 :start 1 :end 6)
         #*011101)))
(define-test sacla-must-sequence.3282 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not (complement #'zerop) #*01010101 :start 1 :end 6 :count 1)
   #*0110101)))
(define-test sacla-must-sequence.3283 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not (complement #'zerop)
                  #*01010101
                  :start 1
                  :end 6
                  :count 1
                  :from-end t)
   #*0101101)))
(define-test sacla-must-sequence.3284 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not (complement #'oddp)
                  #*01010101
                  :start 1
                  :end 6
                  :count 1
                  :from-end t)
   #*0101001)))
(define-test sacla-must-sequence.3285 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not (complement #'evenp)
                  #*01010101
                  :start 1
                  :end 6
                  :count 1
                  :from-end t)
   #*0101101)))
(define-test sacla-must-sequence.3286 (:tag :sacla)
 (assert-true
  (equal
   (remove-if-not (complement #'plusp)
                  #*01010101
                  :start 1
                  :end 6
                  :count 1
                  :from-end t
                  :key #'(lambda (arg) (* arg 10)))
   #*0101001)))
(define-test sacla-must-sequence.3287 (:tag :sacla)
 (assert-true (equal (delete 'a (list 'a 'b 'c 'a 'b 'c)) '(b c b c))))
(define-test sacla-must-sequence.3288 (:tag :sacla)
 (assert-true (equal (delete 'b (list 'a 'b 'c 'a 'b 'c)) '(a c a c))))
(define-test sacla-must-sequence.3289 (:tag :sacla)
 (assert-true (equal (delete 'c (list 'a 'b 'c 'a 'b 'c)) '(a b a b))))
(define-test sacla-must-sequence.3290 (:tag :sacla)
 (assert-true (equal (delete 'a (list 'a 'a 'a)) 'nil)))
(define-test sacla-must-sequence.3291 (:tag :sacla)
 (assert-true (equal (delete 'z (list 'a 'b 'c)) '(a b c))))
(define-test sacla-must-sequence.3292 (:tag :sacla)
 (assert-true (equal (delete 'a 'nil) 'nil)))
(define-test sacla-must-sequence.3293 (:tag :sacla)
 (assert-true
  (equal (delete 'a (list 'a 'b 'c 'a 'b 'c) :count 0) '(a b c a b c))))
(define-test sacla-must-sequence.3294 (:tag :sacla)
 (assert-true
  (equal (delete 'a (list 'a 'b 'c 'a 'b 'c) :count 1) '(b c a b c))))
(define-test sacla-must-sequence.3295 (:tag :sacla)
 (assert-true
  (equal (delete 'a (list 'a 'b 'c 'a 'b 'c) :count 1 :from-end t)
         '(a b c b c))))
(define-test sacla-must-sequence.3296 (:tag :sacla)
 (assert-true (equal (delete 'a (list 'a 'b 'c 'a 'b 'c) :count 2) '(b c b c))))
(define-test sacla-must-sequence.3297 (:tag :sacla)
 (assert-true
  (equal (delete 'a (list 'a 'b 'c 'a 'b 'c) :count 2 :from-end t) '(b c b c))))
(define-test sacla-must-sequence.3298 (:tag :sacla)
 (assert-true (equal (delete 'a (list 'a 'b 'c 'a 'b 'c) :count 3) '(b c b c))))
(define-test sacla-must-sequence.3299 (:tag :sacla)
 (assert-true
  (equal (delete 'a (list 'a 'b 'c 'a 'b 'c) :count 3 :from-end t) '(b c b c))))
(define-test sacla-must-sequence.3300 (:tag :sacla)
 (assert-true (equal (delete 'a (list 'a 'b 'c 'a 'b 'c) :count 4) '(b c b c))))
(define-test sacla-must-sequence.3301 (:tag :sacla)
 (assert-true
  (equal (delete 'a (list 'a 'b 'c 'a 'b 'c) :count 4 :from-end t) '(b c b c))))
(define-test sacla-must-sequence.3302 (:tag :sacla)
 (assert-true
  (equal (delete 'a (list 'a 'b 'c 'a 'b 'c) :count -1) '(a b c a b c))))
(define-test sacla-must-sequence.3303 (:tag :sacla)
 (assert-true
  (equal (delete 'a (list 'a 'b 'c 'a 'b 'c) :count -10) '(a b c a b c))))
(define-test sacla-must-sequence.3304 (:tag :sacla)
 (assert-true
  (equal (delete 'a (list 'a 'b 'c 'a 'b 'c) :count -100) '(a b c a b c))))
(define-test sacla-must-sequence.3305 (:tag :sacla)
 (assert-true
  (equal (delete 'a (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c) :start 1)
         '(a b c b c b c b c))))
(define-test sacla-must-sequence.3306 (:tag :sacla)
 (assert-true
  (equal
   (delete 'a (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c) :start 1 :count 1)
   '(a b c b c a b c a b c))))
(define-test sacla-must-sequence.3307 (:tag :sacla)
 (assert-true
  (equal
   (delete 'a (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c) :start 1 :count 2)
   '(a b c b c b c a b c))))
(define-test sacla-must-sequence.3308 (:tag :sacla)
 (assert-true
  (equal
   (delete 'a
           (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
           :start 1
           :end nil
           :count 2)
   '(a b c b c b c a b c))))
(define-test sacla-must-sequence.3309 (:tag :sacla)
 (assert-true
  (equal (delete 'a (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c) :start 1 :end 8)
         '(a b c b c b c a b c))))
(define-test sacla-must-sequence.3310 (:tag :sacla)
 (assert-true
  (equal
   (delete 'a
           (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
           :start 1
           :end 8
           :count 1)
   '(a b c b c a b c a b c))))
(define-test sacla-must-sequence.3311 (:tag :sacla)
 (assert-true
  (equal
   (delete 'a
           (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
           :start 1
           :end 8
           :count 1
           :from-end t)
   '(a b c a b c b c a b c))))
(define-test sacla-must-sequence.3312 (:tag :sacla)
 (assert-true
  (equal
   (delete 'a
           (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
           :start 1
           :end 8
           :count 0
           :from-end t)
   '(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3313 (:tag :sacla)
 (assert-true
  (equal
   (delete 'a
           (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
           :start 1
           :end 8
           :count -100
           :from-end t)
   '(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3314 (:tag :sacla)
 (assert-true
  (equal (delete 'a (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c) :start 1 :end 1)
         '(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3315 (:tag :sacla)
 (assert-true
  (equal (delete 'a (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c) :start 2 :end 2)
         '(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3316 (:tag :sacla)
 (assert-true
  (equal
   (delete 'a (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c) :start 12 :end 12)
   '(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3317 (:tag :sacla)
 (assert-true
  (equal (delete 'a (list '(a) '(b) '(c) '(a) '(b) '(c)) :key #'car)
         '((b) (c) (b) (c)))))
(define-test sacla-must-sequence.3318 (:tag :sacla)
 (assert-true
  (equal
   (delete 'a
           (list '(a . b) '(b . c) '(c . a) '(a . b) '(b . c) '(c . a))
           :key #'cdr)
   '((a . b) (b . c) (a . b) (b . c)))))
(define-test sacla-must-sequence.3319 (:tag :sacla)
 (assert-true
  (equal
   (delete "love" (list '("Love") '("LOve") '("LOVe") '("LOVE")) :key #'car)
   '(("Love") ("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3320 (:tag :sacla)
 (assert-true
  (equal
   (delete "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :count -10)
   '(("Love") ("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3321 (:tag :sacla)
 (assert-true
  (equal
   (delete "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test #'string-equal)
   'nil)))
(define-test sacla-must-sequence.3322 (:tag :sacla)
 (assert-true
  (equal
   (delete "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test-not (complement #'string-equal))
   'nil)))
(define-test sacla-must-sequence.3323 (:tag :sacla)
 (assert-true
  (equal
   (delete "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test #'string-equal
           :count 1)
   '(("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3324 (:tag :sacla)
 (assert-true
  (equal
   (delete "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test-not (complement #'string-equal)
           :count 1)
   '(("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3325 (:tag :sacla)
 (assert-true
  (equal
   (delete "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test #'string-equal
           :count 1
           :from-end t)
   '(("Love") ("LOve") ("LOVe")))))
(define-test sacla-must-sequence.3326 (:tag :sacla)
 (assert-true
  (equal
   (delete "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test-not (complement #'string-equal)
           :count 1
           :from-end t)
   '(("Love") ("LOve") ("LOVe")))))
(define-test sacla-must-sequence.3327 (:tag :sacla)
 (assert-true
  (equal
   (delete "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test #'string-equal
           :count 2
           :from-end t)
   '(("Love") ("LOve")))))
(define-test sacla-must-sequence.3328 (:tag :sacla)
 (assert-true
  (equal
   (delete "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test-not (complement #'string-equal)
           :count 2
           :from-end t)
   '(("Love") ("LOve")))))
(define-test sacla-must-sequence.3329 (:tag :sacla)
 (assert-true
  (equal
   (delete "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test #'string-equal
           :start 1
           :end 3)
   '(("Love") ("LOVE")))))
(define-test sacla-must-sequence.3330 (:tag :sacla)
 (assert-true
  (equal
   (delete "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test-not (complement #'string-equal)
           :start 1
           :end 3)
   '(("Love") ("LOVE")))))
(define-test sacla-must-sequence.3331 (:tag :sacla)
 (assert-true
  (equal
   (delete "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test #'string-equal
           :count 1
           :start 1
           :end 3)
   '(("Love") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3332 (:tag :sacla)
 (assert-true
  (equal
   (delete "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test-not (complement #'string-equal)
           :count 1
           :start 1
           :end 3)
   '(("Love") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3333 (:tag :sacla)
 (assert-true
  (equal
   (delete "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test #'string-equal
           :count 1
           :from-end t
           :start 1
           :end 3)
   '(("Love") ("LOve") ("LOVE")))))
(define-test sacla-must-sequence.3334 (:tag :sacla)
 (assert-true
  (equal
   (delete "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test-not (complement #'string-equal)
           :count 1
           :from-end t
           :start 1
           :end 3)
   '(("Love") ("LOve") ("LOVE")))))
(define-test sacla-must-sequence.3335 (:tag :sacla)
 (assert-true
  (equal
   (delete "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test #'string-equal
           :count 10
           :from-end t
           :start 1
           :end 3)
   '(("Love") ("LOVE")))))
(define-test sacla-must-sequence.3336 (:tag :sacla)
 (assert-true
  (equal
   (delete "love"
           (list '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test-not (complement #'string-equal)
           :count 10
           :from-end t
           :start 1
           :end 3)
   '(("Love") ("LOVE")))))
(define-test sacla-must-sequence.3337 (:tag :sacla)
 (assert-true (equalp (delete 'a (vector 'a 'b 'c 'a 'b 'c)) #(b c b c))))
(define-test sacla-must-sequence.3338 (:tag :sacla)
 (assert-true (equalp (delete 'b (vector 'a 'b 'c 'a 'b 'c)) #(a c a c))))
(define-test sacla-must-sequence.3339 (:tag :sacla)
 (assert-true (equalp (delete 'c (vector 'a 'b 'c 'a 'b 'c)) #(a b a b))))
(define-test sacla-must-sequence.3340 (:tag :sacla)
 (assert-true (equalp (delete 'a (vector 'a 'a 'a)) #())))
(define-test sacla-must-sequence.3341 (:tag :sacla)
 (assert-true (equalp (delete 'z (vector 'a 'b 'c)) #(a b c))))
(define-test sacla-must-sequence.3342 (:tag :sacla)
 (assert-true (equalp (delete 'a #()) #())))
(define-test sacla-must-sequence.3343 (:tag :sacla)
 (assert-true
  (equalp (delete 'a (vector 'a 'b 'c 'a 'b 'c) :count 0) #(a b c a b c))))
(define-test sacla-must-sequence.3344 (:tag :sacla)
 (assert-true
  (equalp (delete 'a (vector 'a 'b 'c 'a 'b 'c) :count 1) #(b c a b c))))
(define-test sacla-must-sequence.3345 (:tag :sacla)
 (assert-true
  (equalp (delete 'a (vector 'a 'b 'c 'a 'b 'c) :count 1 :from-end t)
          #(a b c b c))))
(define-test sacla-must-sequence.3346 (:tag :sacla)
 (assert-true
  (equalp (delete 'a (vector 'a 'b 'c 'a 'b 'c) :count 2) #(b c b c))))
(define-test sacla-must-sequence.3347 (:tag :sacla)
 (assert-true
  (equalp (delete 'a (vector 'a 'b 'c 'a 'b 'c) :count 2 :from-end t)
          #(b c b c))))
(define-test sacla-must-sequence.3348 (:tag :sacla)
 (assert-true
  (equalp (delete 'a (vector 'a 'b 'c 'a 'b 'c) :count 3) #(b c b c))))
(define-test sacla-must-sequence.3349 (:tag :sacla)
 (assert-true
  (equalp (delete 'a (vector 'a 'b 'c 'a 'b 'c) :count 3 :from-end t)
          #(b c b c))))
(define-test sacla-must-sequence.3350 (:tag :sacla)
 (assert-true
  (equalp (delete 'a (vector 'a 'b 'c 'a 'b 'c) :count 4) #(b c b c))))
(define-test sacla-must-sequence.3351 (:tag :sacla)
 (assert-true
  (equalp (delete 'a (vector 'a 'b 'c 'a 'b 'c) :count 4 :from-end t)
          #(b c b c))))
(define-test sacla-must-sequence.3352 (:tag :sacla)
 (assert-true
  (equalp (delete 'a (vector 'a 'b 'c 'a 'b 'c) :count -1) #(a b c a b c))))
(define-test sacla-must-sequence.3353 (:tag :sacla)
 (assert-true
  (equalp (delete 'a (vector 'a 'b 'c 'a 'b 'c) :count -10) #(a b c a b c))))
(define-test sacla-must-sequence.3354 (:tag :sacla)
 (assert-true
  (equalp (delete 'a (vector 'a 'b 'c 'a 'b 'c) :count -100) #(a b c a b c))))
(define-test sacla-must-sequence.3355 (:tag :sacla)
 (assert-true
  (equalp (delete 'a (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c) :start 1)
          #(a b c b c b c b c))))
(define-test sacla-must-sequence.3356 (:tag :sacla)
 (assert-true
  (equalp
   (delete 'a (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c) :start 1 :count 1)
   #(a b c b c a b c a b c))))
(define-test sacla-must-sequence.3357 (:tag :sacla)
 (assert-true
  (equalp
   (delete 'a (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c) :start 1 :count 2)
   #(a b c b c b c a b c))))
(define-test sacla-must-sequence.3358 (:tag :sacla)
 (assert-true
  (equalp
   (delete 'a
           (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
           :start 1
           :end nil
           :count 2)
   #(a b c b c b c a b c))))
(define-test sacla-must-sequence.3359 (:tag :sacla)
 (assert-true
  (equalp
   (delete 'a (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c) :start 1 :end 8)
   #(a b c b c b c a b c))))
(define-test sacla-must-sequence.3360 (:tag :sacla)
 (assert-true
  (equalp
   (delete 'a
           (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
           :start 1
           :end 8
           :count 1)
   #(a b c b c a b c a b c))))
(define-test sacla-must-sequence.3361 (:tag :sacla)
 (assert-true
  (equalp
   (delete 'a
           (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
           :start 1
           :end 8
           :count 1
           :from-end t)
   #(a b c a b c b c a b c))))
(define-test sacla-must-sequence.3362 (:tag :sacla)
 (assert-true
  (equalp
   (delete 'a
           (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
           :start 1
           :end 8
           :count 0
           :from-end t)
   #(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3363 (:tag :sacla)
 (assert-true
  (equalp
   (delete 'a
           (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
           :start 1
           :end 8
           :count -100
           :from-end t)
   #(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3364 (:tag :sacla)
 (assert-true
  (equalp
   (delete 'a (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c) :start 1 :end 1)
   #(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3365 (:tag :sacla)
 (assert-true
  (equalp
   (delete 'a (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c) :start 2 :end 2)
   #(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3366 (:tag :sacla)
 (assert-true
  (equalp
   (delete 'a (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c) :start 12 :end 12)
   #(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3367 (:tag :sacla)
 (assert-true
  (equalp (delete 'a (vector '(a) '(b) '(c) '(a) '(b) '(c)) :key #'car)
          #((b) (c) (b) (c)))))
(define-test sacla-must-sequence.3368 (:tag :sacla)
 (assert-true
  (equalp
   (delete 'a
           (vector '(a . b) '(b . c) '(c . a) '(a . b) '(b . c) '(c . a))
           :key #'cdr)
   #((a . b) (b . c) (a . b) (b . c)))))
(define-test sacla-must-sequence.3369 (:tag :sacla)
 (assert-true
  (equalp
   (delete "love" (vector '("Love") '("LOve") '("LOVe") '("LOVE")) :key #'car)
   #(("Love") ("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3370 (:tag :sacla)
 (assert-true
  (equalp
   (delete "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :count -10)
   #(("Love") ("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3371 (:tag :sacla)
 (assert-true
  (equalp
   (delete "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test #'string-equal)
   #())))
(define-test sacla-must-sequence.3372 (:tag :sacla)
 (assert-true
  (equalp
   (delete "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test-not (complement #'string-equal))
   #())))
(define-test sacla-must-sequence.3373 (:tag :sacla)
 (assert-true
  (equalp
   (delete "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test #'string-equal
           :count 1)
   #(("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3374 (:tag :sacla)
 (assert-true
  (equalp
   (delete "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test-not (complement #'string-equal)
           :count 1)
   #(("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3375 (:tag :sacla)
 (assert-true
  (equalp
   (delete "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test #'string-equal
           :count 1
           :from-end t)
   #(("Love") ("LOve") ("LOVe")))))
(define-test sacla-must-sequence.3376 (:tag :sacla)
 (assert-true
  (equalp
   (delete "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test-not (complement #'string-equal)
           :count 1
           :from-end t)
   #(("Love") ("LOve") ("LOVe")))))
(define-test sacla-must-sequence.3377 (:tag :sacla)
 (assert-true
  (equalp
   (delete "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test #'string-equal
           :count 2
           :from-end t)
   #(("Love") ("LOve")))))
(define-test sacla-must-sequence.3378 (:tag :sacla)
 (assert-true
  (equalp
   (delete "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test-not (complement #'string-equal)
           :count 2
           :from-end t)
   #(("Love") ("LOve")))))
(define-test sacla-must-sequence.3379 (:tag :sacla)
 (assert-true
  (equalp
   (delete "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test #'string-equal
           :start 1
           :end 3)
   #(("Love") ("LOVE")))))
(define-test sacla-must-sequence.3380 (:tag :sacla)
 (assert-true
  (equalp
   (delete "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test-not (complement #'string-equal)
           :start 1
           :end 3)
   #(("Love") ("LOVE")))))
(define-test sacla-must-sequence.3381 (:tag :sacla)
 (assert-true
  (equalp
   (delete "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test #'string-equal
           :count 1
           :start 1
           :end 3)
   #(("Love") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3382 (:tag :sacla)
 (assert-true
  (equalp
   (delete "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test-not (complement #'string-equal)
           :count 1
           :start 1
           :end 3)
   #(("Love") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3383 (:tag :sacla)
 (assert-true
  (equalp
   (delete "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test #'string-equal
           :count 1
           :from-end t
           :start 1
           :end 3)
   #(("Love") ("LOve") ("LOVE")))))
(define-test sacla-must-sequence.3384 (:tag :sacla)
 (assert-true
  (equalp
   (delete "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test-not (complement #'string-equal)
           :count 1
           :from-end t
           :start 1
           :end 3)
   #(("Love") ("LOve") ("LOVE")))))
(define-test sacla-must-sequence.3385 (:tag :sacla)
 (assert-true
  (equalp
   (delete "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test #'string-equal
           :count 10
           :from-end t
           :start 1
           :end 3)
   #(("Love") ("LOVE")))))
(define-test sacla-must-sequence.3386 (:tag :sacla)
 (assert-true
  (equalp
   (delete "love"
           (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
           :key #'car
           :test-not (complement #'string-equal)
           :count 10
           :from-end t
           :start 1
           :end 3)
   #(("Love") ("LOVE")))))
(define-test sacla-must-sequence.3387 (:tag :sacla)
 (assert-true (string= (delete #\a (copy-seq "abcabc")) "bcbc")))
(define-test sacla-must-sequence.3388 (:tag :sacla)
 (assert-true (string= (delete #\a (copy-seq "")) "")))
(define-test sacla-must-sequence.3389 (:tag :sacla)
 (assert-true (string= (delete #\a (copy-seq "xyz")) "xyz")))
(define-test sacla-must-sequence.3390 (:tag :sacla)
 (assert-true (string= (delete #\a (copy-seq "ABCABC")) "ABCABC")))
(define-test sacla-must-sequence.3391 (:tag :sacla)
 (assert-true
  (string= (delete #\a (copy-seq "ABCABC") :key #'char-downcase) "BCBC")))
(define-test sacla-must-sequence.3392 (:tag :sacla)
 (assert-true (string= (delete #\a (copy-seq "abcabc") :count 1) "bcabc")))
(define-test sacla-must-sequence.3393 (:tag :sacla)
 (assert-true
  (string= (delete #\a (copy-seq "abcabc") :count 1 :from-end t) "abcbc")))
(define-test sacla-must-sequence.3394 (:tag :sacla)
 (assert-true (string= (delete #\a (copy-seq "abcabc") :count 0) "abcabc")))
(define-test sacla-must-sequence.3395 (:tag :sacla)
 (assert-true (string= (delete #\a (copy-seq "abcabc") :count -10) "abcabc")))
(define-test sacla-must-sequence.3396 (:tag :sacla)
 (assert-true (string= (delete #\a (copy-seq "abcabc") :count 0) "abcabc")))
(define-test sacla-must-sequence.3397 (:tag :sacla)
 (assert-true (string= (delete #\a (copy-seq "abcabc")) "bcbc")))
(define-test sacla-must-sequence.3398 (:tag :sacla)
 (assert-true (string= (delete #\b (copy-seq "abcabc")) "acac")))
(define-test sacla-must-sequence.3399 (:tag :sacla)
 (assert-true (string= (delete #\c (copy-seq "abcabc")) "abab")))
(define-test sacla-must-sequence.3400 (:tag :sacla)
 (assert-true (string= (delete #\a (copy-seq "abcabc") :count 0) "abcabc")))
(define-test sacla-must-sequence.3401 (:tag :sacla)
 (assert-true (string= (delete #\a (copy-seq "abcabc") :count 1) "bcabc")))
(define-test sacla-must-sequence.3402 (:tag :sacla)
 (assert-true
  (string= (delete #\a (copy-seq "abcabc") :count 1 :from-end t) "abcbc")))
(define-test sacla-must-sequence.3403 (:tag :sacla)
 (assert-true (string= (delete #\a (copy-seq "abcabc") :count 2) "bcbc")))
(define-test sacla-must-sequence.3404 (:tag :sacla)
 (assert-true
  (string= (delete #\a (copy-seq "abcabc") :count 2 :from-end t) "bcbc")))
(define-test sacla-must-sequence.3405 (:tag :sacla)
 (assert-true (string= (delete #\a (copy-seq "abcabc") :count 3) "bcbc")))
(define-test sacla-must-sequence.3406 (:tag :sacla)
 (assert-true
  (string= (delete #\a (copy-seq "abcabc") :count 3 :from-end t) "bcbc")))
(define-test sacla-must-sequence.3407 (:tag :sacla)
 (assert-true (string= (delete #\a (copy-seq "abcabc") :count 4) "bcbc")))
(define-test sacla-must-sequence.3408 (:tag :sacla)
 (assert-true
  (string= (delete #\a (copy-seq "abcabc") :count 4 :from-end t) "bcbc")))
(define-test sacla-must-sequence.3409 (:tag :sacla)
 (assert-true (string= (delete #\a (copy-seq "abcabc") :count -1) "abcabc")))
(define-test sacla-must-sequence.3410 (:tag :sacla)
 (assert-true (string= (delete #\a (copy-seq "abcabc") :count -10) "abcabc")))
(define-test sacla-must-sequence.3411 (:tag :sacla)
 (assert-true (string= (delete #\a (copy-seq "abcabc") :count -100) "abcabc")))
(define-test sacla-must-sequence.3412 (:tag :sacla)
 (assert-true
  (string= (delete #\a (copy-seq "abcabcabcabc") :start 1) "abcbcbcbc")))
(define-test sacla-must-sequence.3413 (:tag :sacla)
 (assert-true
  (string= (delete #\a (copy-seq "abcabcabcabc") :start 1 :count 1)
           "abcbcabcabc")))
(define-test sacla-must-sequence.3414 (:tag :sacla)
 (assert-true
  (string= (delete #\a (copy-seq "abcabcabcabc") :start 1 :count 2)
           "abcbcbcabc")))
(define-test sacla-must-sequence.3415 (:tag :sacla)
 (assert-true
  (string= (delete #\a (copy-seq "abcabcabcabc") :start 1 :end nil :count 2)
           "abcbcbcabc")))
(define-test sacla-must-sequence.3416 (:tag :sacla)
 (assert-true
  (string= (delete #\a (copy-seq "abcabcabcabc") :start 1 :end 8)
           "abcbcbcabc")))
(define-test sacla-must-sequence.3417 (:tag :sacla)
 (assert-true
  (string= (delete #\a (copy-seq "abcabcabcabc") :start 1 :end 8 :count 1)
           "abcbcabcabc")))
(define-test sacla-must-sequence.3418 (:tag :sacla)
 (assert-true
  (string=
   (delete #\a (copy-seq "abcabcabcabc") :start 1 :end 8 :count 1 :from-end t)
   "abcabcbcabc")))
(define-test sacla-must-sequence.3419 (:tag :sacla)
 (assert-true
  (string=
   (delete #\a (copy-seq "abcabcabcabc") :start 1 :end 8 :count 0 :from-end t)
   "abcabcabcabc")))
(define-test sacla-must-sequence.3420 (:tag :sacla)
 (assert-true
  (string=
   (delete #\a
           (copy-seq "abcabcabcabc")
           :start 1
           :end 8
           :count -100
           :from-end t)
   "abcabcabcabc")))
(define-test sacla-must-sequence.3421 (:tag :sacla)
 (assert-true
  (string= (delete #\a (copy-seq "abcabcabcabc") :start 1 :end 1)
           "abcabcabcabc")))
(define-test sacla-must-sequence.3422 (:tag :sacla)
 (assert-true
  (string= (delete #\a (copy-seq "abcabcabcabc") :start 2 :end 2)
           "abcabcabcabc")))
(define-test sacla-must-sequence.3423 (:tag :sacla)
 (assert-true
  (string= (delete #\a (copy-seq "abcabcabcabc") :start 12 :end 12)
           "abcabcabcabc")))
(define-test sacla-must-sequence.3424 (:tag :sacla)
 (assert-true (equal (delete 0 #*0101) #*11)))
(define-test sacla-must-sequence.3425 (:tag :sacla)
 (assert-true (equal (delete 0 #*01010101 :count 1) #*1010101)))
(define-test sacla-must-sequence.3426 (:tag :sacla)
 (assert-true (equal (delete 0 #*01010101 :count 1 :from-end t) #*0101011)))
(define-test sacla-must-sequence.3427 (:tag :sacla)
 (assert-true (equal (delete 0 #*01010101 :start 1) #*01111)))
(define-test sacla-must-sequence.3428 (:tag :sacla)
 (assert-true (equal (delete 0 #*01010101 :start 1 :end nil) #*01111)))
(define-test sacla-must-sequence.3429 (:tag :sacla)
 (assert-true (equal (delete 0 #*01010101 :start 1 :end 6) #*011101)))
(define-test sacla-must-sequence.3430 (:tag :sacla)
 (assert-true (equal (delete 0 #*01010101 :start 1 :end 6 :count 1) #*0110101)))
(define-test sacla-must-sequence.3431 (:tag :sacla)
 (assert-true
  (equal (delete 0 #*01010101 :start 1 :end 6 :count 1 :from-end t) #*0101101)))
(define-test sacla-must-sequence.3432 (:tag :sacla)
 (assert-true
  (equal
   (delete 0
           #*01010101
           :start 1
           :end 6
           :count 1
           :from-end t
           :test #'(lambda (a b) (declare (ignore a)) (oddp b)))
   #*0101001)))
(define-test sacla-must-sequence.3433 (:tag :sacla)
 (assert-true
  (equal
   (delete 0
           #*01010101
           :start 1
           :end 6
           :count 1
           :from-end t
           :test-not #'(lambda (a b) (declare (ignore a)) (evenp b)))
   #*0101001)))
(define-test sacla-must-sequence.3434 (:tag :sacla)
 (assert-true
  (equal
   (delete 0
           #*01010101
           :start 1
           :end 6
           :count 1
           :from-end t
           :test #'(lambda (a b) (declare (ignore a)) (evenp b)))
   #*0101101)))
(define-test sacla-must-sequence.3435 (:tag :sacla)
 (assert-true
  (equal
   (delete 0
           #*01010101
           :start 1
           :end 6
           :count 1
           :from-end t
           :test-not #'(lambda (a b) (declare (ignore a)) (oddp b)))
   #*0101101)))
(define-test sacla-must-sequence.3436 (:tag :sacla)
 (assert-true
  (equal
   (delete 0
           #*01010101
           :start 1
           :end 6
           :count 1
           :from-end t
           :key #'(lambda (arg) (* arg 10))
           :test #'(lambda (a b) (declare (ignore a)) (> b 1)))
   #*0101001)))
(define-test sacla-must-sequence.3437 (:tag :sacla)
 (assert-true
  (equal (delete-if #'(lambda (arg) (eql arg 'a)) (list 'a 'b 'c 'a 'b 'c))
         '(b c b c))))
(define-test sacla-must-sequence.3438 (:tag :sacla)
 (assert-true
  (equal (delete-if #'(lambda (arg) (eql arg 'b)) (list 'a 'b 'c 'a 'b 'c))
         '(a c a c))))
(define-test sacla-must-sequence.3439 (:tag :sacla)
 (assert-true
  (equal (delete-if #'(lambda (arg) (eql arg 'c)) (list 'a 'b 'c 'a 'b 'c))
         '(a b a b))))
(define-test sacla-must-sequence.3440 (:tag :sacla)
 (assert-true
  (equal (delete-if #'(lambda (arg) (eql arg 'a)) (list 'a 'a 'a)) 'nil)))
(define-test sacla-must-sequence.3441 (:tag :sacla)
 (assert-true
  (equal (delete-if #'(lambda (arg) (eql arg 'z)) (list 'a 'b 'c)) '(a b c))))
(define-test sacla-must-sequence.3442 (:tag :sacla)
 (assert-true (equal (delete-if #'(lambda (arg) (eql arg 'a)) 'nil) 'nil)))
(define-test sacla-must-sequence.3443 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (eql arg 'a)) (list 'a 'b 'c 'a 'b 'c) :count 0)
   '(a b c a b c))))
(define-test sacla-must-sequence.3444 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (eql arg 'a)) (list 'a 'b 'c 'a 'b 'c) :count 1)
   '(b c a b c))))
(define-test sacla-must-sequence.3445 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c)
              :count 1
              :from-end t)
   '(a b c b c))))
(define-test sacla-must-sequence.3446 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (eql arg 'a)) (list 'a 'b 'c 'a 'b 'c) :count 2)
   '(b c b c))))
(define-test sacla-must-sequence.3447 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c)
              :count 2
              :from-end t)
   '(b c b c))))
(define-test sacla-must-sequence.3448 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (eql arg 'a)) (list 'a 'b 'c 'a 'b 'c) :count 3)
   '(b c b c))))
(define-test sacla-must-sequence.3449 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c)
              :count 3
              :from-end t)
   '(b c b c))))
(define-test sacla-must-sequence.3450 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (eql arg 'a)) (list 'a 'b 'c 'a 'b 'c) :count 4)
   '(b c b c))))
(define-test sacla-must-sequence.3451 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c)
              :count 4
              :from-end t)
   '(b c b c))))
(define-test sacla-must-sequence.3452 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (eql arg 'a)) (list 'a 'b 'c 'a 'b 'c) :count -1)
   '(a b c a b c))))
(define-test sacla-must-sequence.3453 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c)
              :count -10)
   '(a b c a b c))))
(define-test sacla-must-sequence.3454 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c)
              :count -100)
   '(a b c a b c))))
(define-test sacla-must-sequence.3455 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1)
   '(a b c b c b c b c))))
(define-test sacla-must-sequence.3456 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :count 1)
   '(a b c b c a b c a b c))))
(define-test sacla-must-sequence.3457 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :count 2)
   '(a b c b c b c a b c))))
(define-test sacla-must-sequence.3458 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :end nil
              :count 2)
   '(a b c b c b c a b c))))
(define-test sacla-must-sequence.3459 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :end 8)
   '(a b c b c b c a b c))))
(define-test sacla-must-sequence.3460 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :end 8
              :count 1)
   '(a b c b c a b c a b c))))
(define-test sacla-must-sequence.3461 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :end 8
              :count 1
              :from-end t)
   '(a b c a b c b c a b c))))
(define-test sacla-must-sequence.3462 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :end 8
              :count 0
              :from-end t)
   '(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3463 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :end 8
              :count -100
              :from-end t)
   '(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3464 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :end 1)
   '(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3465 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 2
              :end 2)
   '(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3466 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (eql arg 'a))
              (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 12
              :end 12)
   '(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3467 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (eql arg 'a))
              (list '(a) '(b) '(c) '(a) '(b) '(c))
              :key #'car)
   '((b) (c) (b) (c)))))
(define-test sacla-must-sequence.3468 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (eql arg 'a))
              (list '(a . b) '(b . c) '(c . a) '(a . b) '(b . c) '(c . a))
              :key #'cdr)
   '((a . b) (b . c) (a . b) (b . c)))))
(define-test sacla-must-sequence.3469 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (eql arg "love"))
              (list '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car)
   '(("Love") ("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3470 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (eql arg "love"))
              (list '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count -10)
   '(("Love") ("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3471 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (string-equal arg "love"))
              (list '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car)
   'nil)))
(define-test sacla-must-sequence.3472 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (string-equal arg "love"))
              (list '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count 1)
   '(("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3473 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (string-equal arg "love"))
              (list '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count 1
              :from-end t)
   '(("Love") ("LOve") ("LOVe")))))
(define-test sacla-must-sequence.3474 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (string-equal arg "love"))
              (list '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count 2
              :from-end t)
   '(("Love") ("LOve")))))
(define-test sacla-must-sequence.3475 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (string-equal arg "love"))
              (list '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :start 1
              :end 3)
   '(("Love") ("LOVE")))))
(define-test sacla-must-sequence.3476 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (string-equal arg "love"))
              (list '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count 1
              :start 1
              :end 3)
   '(("Love") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3477 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (string-equal arg "love"))
              (list '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count 1
              :from-end t
              :start 1
              :end 3)
   '(("Love") ("LOve") ("LOVE")))))
(define-test sacla-must-sequence.3478 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'(lambda (arg) (string-equal arg "love"))
              (list '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count 10
              :from-end t
              :start 1
              :end 3)
   '(("Love") ("LOVE")))))
(define-test sacla-must-sequence.3479 (:tag :sacla)
 (assert-true
  (equalp (delete-if #'(lambda (arg) (eql arg 'a)) (vector 'a 'b 'c 'a 'b 'c))
          #(b c b c))))
(define-test sacla-must-sequence.3480 (:tag :sacla)
 (assert-true
  (equalp (delete-if #'(lambda (arg) (eql arg 'b)) (vector 'a 'b 'c 'a 'b 'c))
          #(a c a c))))
(define-test sacla-must-sequence.3481 (:tag :sacla)
 (assert-true
  (equalp (delete-if #'(lambda (arg) (eql arg 'c)) (vector 'a 'b 'c 'a 'b 'c))
          #(a b a b))))
(define-test sacla-must-sequence.3482 (:tag :sacla)
 (assert-true
  (equalp (delete-if #'(lambda (arg) (eql arg 'a)) (vector 'a 'a 'a)) #())))
(define-test sacla-must-sequence.3483 (:tag :sacla)
 (assert-true
  (equalp (delete-if #'(lambda (arg) (eql arg 'z)) (vector 'a 'b 'c))
          #(a b c))))
(define-test sacla-must-sequence.3484 (:tag :sacla)
 (assert-true (equalp (delete-if #'(lambda (arg) (eql arg 'a)) #()) #())))
(define-test sacla-must-sequence.3485 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c)
              :count 0)
   #(a b c a b c))))
(define-test sacla-must-sequence.3486 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c)
              :count 1)
   #(b c a b c))))
(define-test sacla-must-sequence.3487 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c)
              :count 1
              :from-end t)
   #(a b c b c))))
(define-test sacla-must-sequence.3488 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c)
              :count 2)
   #(b c b c))))
(define-test sacla-must-sequence.3489 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c)
              :count 2
              :from-end t)
   #(b c b c))))
(define-test sacla-must-sequence.3490 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c)
              :count 3)
   #(b c b c))))
(define-test sacla-must-sequence.3491 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c)
              :count 3
              :from-end t)
   #(b c b c))))
(define-test sacla-must-sequence.3492 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c)
              :count 4)
   #(b c b c))))
(define-test sacla-must-sequence.3493 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c)
              :count 4
              :from-end t)
   #(b c b c))))
(define-test sacla-must-sequence.3494 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c)
              :count -1)
   #(a b c a b c))))
(define-test sacla-must-sequence.3495 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c)
              :count -10)
   #(a b c a b c))))
(define-test sacla-must-sequence.3496 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c)
              :count -100)
   #(a b c a b c))))
(define-test sacla-must-sequence.3497 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1)
   #(a b c b c b c b c))))
(define-test sacla-must-sequence.3498 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :count 1)
   #(a b c b c a b c a b c))))
(define-test sacla-must-sequence.3499 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :count 2)
   #(a b c b c b c a b c))))
(define-test sacla-must-sequence.3500 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :end nil
              :count 2)
   #(a b c b c b c a b c))))
(define-test sacla-must-sequence.3501 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :end 8)
   #(a b c b c b c a b c))))
(define-test sacla-must-sequence.3502 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :end 8
              :count 1)
   #(a b c b c a b c a b c))))
(define-test sacla-must-sequence.3503 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :end 8
              :count 1
              :from-end t)
   #(a b c a b c b c a b c))))
(define-test sacla-must-sequence.3504 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :end 8
              :count 0
              :from-end t)
   #(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3505 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :end 8
              :count -100
              :from-end t)
   #(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3506 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 1
              :end 1)
   #(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3507 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 2
              :end 2)
   #(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3508 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (eql arg 'a))
              (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
              :start 12
              :end 12)
   #(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3509 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (eql arg 'a))
              (vector '(a) '(b) '(c) '(a) '(b) '(c))
              :key #'car)
   #((b) (c) (b) (c)))))
(define-test sacla-must-sequence.3510 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (eql arg 'a))
              (vector '(a . b) '(b . c) '(c . a) '(a . b) '(b . c) '(c . a))
              :key #'cdr)
   #((a . b) (b . c) (a . b) (b . c)))))
(define-test sacla-must-sequence.3511 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (eql arg "love"))
              (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car)
   #(("Love") ("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3512 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (eql arg "love"))
              (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count -10)
   #(("Love") ("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3513 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (string-equal arg "love"))
              (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car)
   #())))
(define-test sacla-must-sequence.3514 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (string-equal arg "love"))
              (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car)
   #())))
(define-test sacla-must-sequence.3515 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (string-equal arg "love"))
              (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count 1)
   #(("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3516 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (string-equal arg "love"))
              (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count 1)
   #(("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3517 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (string-equal arg "love"))
              (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count 1
              :from-end t)
   #(("Love") ("LOve") ("LOVe")))))
(define-test sacla-must-sequence.3518 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (string-equal arg "love"))
              (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count 1
              :from-end t)
   #(("Love") ("LOve") ("LOVe")))))
(define-test sacla-must-sequence.3519 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (string-equal arg "love"))
              (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count 2
              :from-end t)
   #(("Love") ("LOve")))))
(define-test sacla-must-sequence.3520 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (string-equal arg "love"))
              (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count 2
              :from-end t)
   #(("Love") ("LOve")))))
(define-test sacla-must-sequence.3521 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (string-equal arg "love"))
              (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :start 1
              :end 3)
   #(("Love") ("LOVE")))))
(define-test sacla-must-sequence.3522 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (string-equal arg "love"))
              (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count 1
              :start 1
              :end 3)
   #(("Love") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3523 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (string-equal arg "love"))
              (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count 1
              :from-end t
              :start 1
              :end 3)
   #(("Love") ("LOve") ("LOVE")))))
(define-test sacla-must-sequence.3524 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if #'(lambda (arg) (string-equal arg "love"))
              (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
              :key #'car
              :count 10
              :from-end t
              :start 1
              :end 3)
   #(("Love") ("LOVE")))))
(define-test sacla-must-sequence.3525 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (string-equal arg #\a)) (copy-seq "abcabc"))
   "bcbc")))
(define-test sacla-must-sequence.3526 (:tag :sacla)
 (assert-true
  (string= (delete-if #'(lambda (arg) (string-equal arg #\a)) (copy-seq ""))
           "")))
(define-test sacla-must-sequence.3527 (:tag :sacla)
 (assert-true
  (string= (delete-if #'(lambda (arg) (string-equal arg #\a)) (copy-seq "xyz"))
           "xyz")))
(define-test sacla-must-sequence.3528 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "ABCABC")
              :key #'char-downcase)
   "BCBC")))
(define-test sacla-must-sequence.3529 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count 1)
   "bcabc")))
(define-test sacla-must-sequence.3530 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count 1
              :from-end t)
   "abcbc")))
(define-test sacla-must-sequence.3531 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count 0)
   "abcabc")))
(define-test sacla-must-sequence.3532 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count -10)
   "abcabc")))
(define-test sacla-must-sequence.3533 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count 0)
   "abcabc")))
(define-test sacla-must-sequence.3534 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (string-equal arg #\a)) (copy-seq "abcabc"))
   "bcbc")))
(define-test sacla-must-sequence.3535 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (string-equal arg #\b)) (copy-seq "abcabc"))
   "acac")))
(define-test sacla-must-sequence.3536 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (string-equal arg #\c)) (copy-seq "abcabc"))
   "abab")))
(define-test sacla-must-sequence.3537 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count 0)
   "abcabc")))
(define-test sacla-must-sequence.3538 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count 1)
   "bcabc")))
(define-test sacla-must-sequence.3539 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count 1
              :from-end t)
   "abcbc")))
(define-test sacla-must-sequence.3540 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count 2)
   "bcbc")))
(define-test sacla-must-sequence.3541 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count 2
              :from-end t)
   "bcbc")))
(define-test sacla-must-sequence.3542 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count 3)
   "bcbc")))
(define-test sacla-must-sequence.3543 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count 3
              :from-end t)
   "bcbc")))
(define-test sacla-must-sequence.3544 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count 4)
   "bcbc")))
(define-test sacla-must-sequence.3545 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count 4
              :from-end t)
   "bcbc")))
(define-test sacla-must-sequence.3546 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count -1)
   "abcabc")))
(define-test sacla-must-sequence.3547 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count -10)
   "abcabc")))
(define-test sacla-must-sequence.3548 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabc")
              :count -100)
   "abcabc")))
(define-test sacla-must-sequence.3549 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabcabcabc")
              :start 1)
   "abcbcbcbc")))
(define-test sacla-must-sequence.3550 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (string-equal arg #\a))
              (copy-seq "abcabcabcabc")
              :start 1
              :count 1)
   "abcbcabcabc")))
(define-test sacla-must-sequence.3551 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (eql arg #\a))
              (copy-seq "abcabcabcabc")
              :start 1
              :count 2)
   "abcbcbcabc")))
(define-test sacla-must-sequence.3552 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (eql arg #\a))
              (copy-seq "abcabcabcabc")
              :start 1
              :end nil
              :count 2)
   "abcbcbcabc")))
(define-test sacla-must-sequence.3553 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (eql arg #\a))
              (copy-seq "abcabcabcabc")
              :start 1
              :end 8)
   "abcbcbcabc")))
(define-test sacla-must-sequence.3554 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (eql arg #\a))
              (copy-seq "abcabcabcabc")
              :start 1
              :end 8
              :count 1)
   "abcbcabcabc")))
(define-test sacla-must-sequence.3555 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (eql arg #\a))
              (copy-seq "abcabcabcabc")
              :start 1
              :end 8
              :count 1
              :from-end t)
   "abcabcbcabc")))
(define-test sacla-must-sequence.3556 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (eql arg #\a))
              (copy-seq "abcabcabcabc")
              :start 1
              :end 8
              :count 0
              :from-end t)
   "abcabcabcabc")))
(define-test sacla-must-sequence.3557 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (eql arg #\a))
              (copy-seq "abcabcabcabc")
              :start 1
              :end 8
              :count -100
              :from-end t)
   "abcabcabcabc")))
(define-test sacla-must-sequence.3558 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (eql arg #\a))
              (copy-seq "abcabcabcabc")
              :start 1
              :end 1)
   "abcabcabcabc")))
(define-test sacla-must-sequence.3559 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (eql arg #\a))
              (copy-seq "abcabcabcabc")
              :start 2
              :end 2)
   "abcabcabcabc")))
(define-test sacla-must-sequence.3560 (:tag :sacla)
 (assert-true
  (string=
   (delete-if #'(lambda (arg) (eql arg #\a))
              (copy-seq "abcabcabcabc")
              :start 12
              :end 12)
   "abcabcabcabc")))
(define-test sacla-must-sequence.3561 (:tag :sacla)
 (assert-true (equal (delete-if #'zerop #*0101) #*11)))
(define-test sacla-must-sequence.3562 (:tag :sacla)
 (assert-true (equal (delete-if #'zerop #*01010101 :count 1) #*1010101)))
(define-test sacla-must-sequence.3563 (:tag :sacla)
 (assert-true
  (equal (delete-if #'zerop #*01010101 :count 1 :from-end t) #*0101011)))
(define-test sacla-must-sequence.3564 (:tag :sacla)
 (assert-true (equal (delete-if #'zerop #*01010101 :start 1) #*01111)))
(define-test sacla-must-sequence.3565 (:tag :sacla)
 (assert-true (equal (delete-if #'zerop #*01010101 :start 1 :end nil) #*01111)))
(define-test sacla-must-sequence.3566 (:tag :sacla)
 (assert-true (equal (delete-if #'zerop #*01010101 :start 1 :end 6) #*011101)))
(define-test sacla-must-sequence.3567 (:tag :sacla)
 (assert-true
  (equal (delete-if #'zerop #*01010101 :start 1 :end 6 :count 1) #*0110101)))
(define-test sacla-must-sequence.3568 (:tag :sacla)
 (assert-true
  (equal (delete-if #'zerop #*01010101 :start 1 :end 6 :count 1 :from-end t)
         #*0101101)))
(define-test sacla-must-sequence.3569 (:tag :sacla)
 (assert-true
  (equal (delete-if #'oddp #*01010101 :start 1 :end 6 :count 1 :from-end t)
         #*0101001)))
(define-test sacla-must-sequence.3570 (:tag :sacla)
 (assert-true
  (equal (delete-if #'evenp #*01010101 :start 1 :end 6 :count 1 :from-end t)
         #*0101101)))
(define-test sacla-must-sequence.3571 (:tag :sacla)
 (assert-true
  (equal
   (delete-if #'plusp
              #*01010101
              :start 1
              :end 6
              :count 1
              :from-end t
              :key #'(lambda (arg) (* arg 10)))
   #*0101001)))
(define-test sacla-must-sequence.3572 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (eql arg 'a))) (list 'a 'b 'c 'a 'b 'c))
   '(b c b c))))
(define-test sacla-must-sequence.3573 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (eql arg 'b))) (list 'a 'b 'c 'a 'b 'c))
   '(a c a c))))
(define-test sacla-must-sequence.3574 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (eql arg 'c))) (list 'a 'b 'c 'a 'b 'c))
   '(a b a b))))
(define-test sacla-must-sequence.3575 (:tag :sacla)
 (assert-true
  (equal (delete-if-not #'(lambda (arg) (not (eql arg 'a))) (list 'a 'a 'a))
         'nil)))
(define-test sacla-must-sequence.3576 (:tag :sacla)
 (assert-true
  (equal (delete-if-not #'(lambda (arg) (not (eql arg 'z))) (list 'a 'b 'c))
         '(a b c))))
(define-test sacla-must-sequence.3577 (:tag :sacla)
 (assert-true
  (equal (delete-if-not #'(lambda (arg) (not (eql arg 'a))) 'nil) 'nil)))
(define-test sacla-must-sequence.3578 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c)
                  :count 0)
   '(a b c a b c))))
(define-test sacla-must-sequence.3579 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c)
                  :count 1)
   '(b c a b c))))
(define-test sacla-must-sequence.3580 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c)
                  :count 1
                  :from-end t)
   '(a b c b c))))
(define-test sacla-must-sequence.3581 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c)
                  :count 2)
   '(b c b c))))
(define-test sacla-must-sequence.3582 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c)
                  :count 2
                  :from-end t)
   '(b c b c))))
(define-test sacla-must-sequence.3583 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c)
                  :count 3)
   '(b c b c))))
(define-test sacla-must-sequence.3584 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c)
                  :count 3
                  :from-end t)
   '(b c b c))))
(define-test sacla-must-sequence.3585 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c)
                  :count 4)
   '(b c b c))))
(define-test sacla-must-sequence.3586 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c)
                  :count 4
                  :from-end t)
   '(b c b c))))
(define-test sacla-must-sequence.3587 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c)
                  :count -1)
   '(a b c a b c))))
(define-test sacla-must-sequence.3588 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c)
                  :count -10)
   '(a b c a b c))))
(define-test sacla-must-sequence.3589 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c)
                  :count -100)
   '(a b c a b c))))
(define-test sacla-must-sequence.3590 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1)
   '(a b c b c b c b c))))
(define-test sacla-must-sequence.3591 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :count 1)
   '(a b c b c a b c a b c))))
(define-test sacla-must-sequence.3592 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :count 2)
   '(a b c b c b c a b c))))
(define-test sacla-must-sequence.3593 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :end nil
                  :count 2)
   '(a b c b c b c a b c))))
(define-test sacla-must-sequence.3594 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :end 8)
   '(a b c b c b c a b c))))
(define-test sacla-must-sequence.3595 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :end 8
                  :count 1)
   '(a b c b c a b c a b c))))
(define-test sacla-must-sequence.3596 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :end 8
                  :count 1
                  :from-end t)
   '(a b c a b c b c a b c))))
(define-test sacla-must-sequence.3597 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :end 8
                  :count 0
                  :from-end t)
   '(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3598 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :end 8
                  :count -100
                  :from-end t)
   '(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3599 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :end 1)
   '(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3600 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 2
                  :end 2)
   '(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3601 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 12
                  :end 12)
   '(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3602 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list '(a) '(b) '(c) '(a) '(b) '(c))
                  :key #'car)
   '((b) (c) (b) (c)))))
(define-test sacla-must-sequence.3603 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (list '(a . b) '(b . c) '(c . a) '(a . b) '(b . c) '(c . a))
                  :key #'cdr)
   '((a . b) (b . c) (a . b) (b . c)))))
(define-test sacla-must-sequence.3604 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (eql arg "love")))
                  (list '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car)
   '(("Love") ("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3605 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (eql arg "love")))
                  (list '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count -10)
   '(("Love") ("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3606 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (list '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car)
   'nil)))
(define-test sacla-must-sequence.3607 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (list '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count 1)
   '(("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3608 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (list '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count 1
                  :from-end t)
   '(("Love") ("LOve") ("LOVe")))))
(define-test sacla-must-sequence.3609 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (list '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count 2
                  :from-end t)
   '(("Love") ("LOve")))))
(define-test sacla-must-sequence.3610 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (list '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :start 1
                  :end 3)
   '(("Love") ("LOVE")))))
(define-test sacla-must-sequence.3611 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (list '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count 1
                  :start 1
                  :end 3)
   '(("Love") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3612 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (list '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count 1
                  :from-end t
                  :start 1
                  :end 3)
   '(("Love") ("LOve") ("LOVE")))))
(define-test sacla-must-sequence.3613 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (list '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count 10
                  :from-end t
                  :start 1
                  :end 3)
   '(("Love") ("LOVE")))))
(define-test sacla-must-sequence.3614 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c))
   #(b c b c))))
(define-test sacla-must-sequence.3615 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (eql arg 'b)))
                  (vector 'a 'b 'c 'a 'b 'c))
   #(a c a c))))
(define-test sacla-must-sequence.3616 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (eql arg 'c)))
                  (vector 'a 'b 'c 'a 'b 'c))
   #(a b a b))))
(define-test sacla-must-sequence.3617 (:tag :sacla)
 (assert-true
  (equalp (delete-if-not #'(lambda (arg) (not (eql arg 'a))) (vector 'a 'a 'a))
          #())))
(define-test sacla-must-sequence.3618 (:tag :sacla)
 (assert-true
  (equalp (delete-if-not #'(lambda (arg) (not (eql arg 'z))) (vector 'a 'b 'c))
          #(a b c))))
(define-test sacla-must-sequence.3619 (:tag :sacla)
 (assert-true
  (equalp (delete-if-not #'(lambda (arg) (not (eql arg 'a))) #()) #())))
(define-test sacla-must-sequence.3620 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c)
                  :count 0)
   #(a b c a b c))))
(define-test sacla-must-sequence.3621 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c)
                  :count 1)
   #(b c a b c))))
(define-test sacla-must-sequence.3622 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c)
                  :count 1
                  :from-end t)
   #(a b c b c))))
(define-test sacla-must-sequence.3623 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c)
                  :count 2)
   #(b c b c))))
(define-test sacla-must-sequence.3624 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c)
                  :count 2
                  :from-end t)
   #(b c b c))))
(define-test sacla-must-sequence.3625 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c)
                  :count 3)
   #(b c b c))))
(define-test sacla-must-sequence.3626 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c)
                  :count 3
                  :from-end t)
   #(b c b c))))
(define-test sacla-must-sequence.3627 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c)
                  :count 4)
   #(b c b c))))
(define-test sacla-must-sequence.3628 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c)
                  :count 4
                  :from-end t)
   #(b c b c))))
(define-test sacla-must-sequence.3629 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c)
                  :count -1)
   #(a b c a b c))))
(define-test sacla-must-sequence.3630 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c)
                  :count -10)
   #(a b c a b c))))
(define-test sacla-must-sequence.3631 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c)
                  :count -100)
   #(a b c a b c))))
(define-test sacla-must-sequence.3632 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1)
   #(a b c b c b c b c))))
(define-test sacla-must-sequence.3633 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :count 1)
   #(a b c b c a b c a b c))))
(define-test sacla-must-sequence.3634 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :count 2)
   #(a b c b c b c a b c))))
(define-test sacla-must-sequence.3635 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :end nil
                  :count 2)
   #(a b c b c b c a b c))))
(define-test sacla-must-sequence.3636 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :end 8)
   #(a b c b c b c a b c))))
(define-test sacla-must-sequence.3637 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :end 8
                  :count 1)
   #(a b c b c a b c a b c))))
(define-test sacla-must-sequence.3638 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :end 8
                  :count 1
                  :from-end t)
   #(a b c a b c b c a b c))))
(define-test sacla-must-sequence.3639 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :end 8
                  :count 0
                  :from-end t)
   #(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3640 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :end 8
                  :count -100
                  :from-end t)
   #(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3641 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 1
                  :end 1)
   #(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3642 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 2
                  :end 2)
   #(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3643 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector 'a 'b 'c 'a 'b 'c 'a 'b 'c 'a 'b 'c)
                  :start 12
                  :end 12)
   #(a b c a b c a b c a b c))))
(define-test sacla-must-sequence.3644 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector '(a) '(b) '(c) '(a) '(b) '(c))
                  :key #'car)
   #((b) (c) (b) (c)))))
(define-test sacla-must-sequence.3645 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (eql arg 'a)))
                  (vector '(a . b)
                          '(b . c)
                          '(c . a)
                          '(a . b)
                          '(b . c)
                          '(c . a))
                  :key #'cdr)
   #((a . b) (b . c) (a . b) (b . c)))))
(define-test sacla-must-sequence.3646 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (eql arg "love")))
                  (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car)
   #(("Love") ("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3647 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (eql arg "love")))
                  (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count -10)
   #(("Love") ("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3648 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car)
   #())))
(define-test sacla-must-sequence.3649 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car)
   #())))
(define-test sacla-must-sequence.3650 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count 1)
   #(("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3651 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count 1)
   #(("LOve") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3652 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count 1
                  :from-end t)
   #(("Love") ("LOve") ("LOVe")))))
(define-test sacla-must-sequence.3653 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count 1
                  :from-end t)
   #(("Love") ("LOve") ("LOVe")))))
(define-test sacla-must-sequence.3654 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count 2
                  :from-end t)
   #(("Love") ("LOve")))))
(define-test sacla-must-sequence.3655 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count 2
                  :from-end t)
   #(("Love") ("LOve")))))
(define-test sacla-must-sequence.3656 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :start 1
                  :end 3)
   #(("Love") ("LOVE")))))
(define-test sacla-must-sequence.3657 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count 1
                  :start 1
                  :end 3)
   #(("Love") ("LOVe") ("LOVE")))))
(define-test sacla-must-sequence.3658 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count 1
                  :from-end t
                  :start 1
                  :end 3)
   #(("Love") ("LOve") ("LOVE")))))
(define-test sacla-must-sequence.3659 (:tag :sacla)
 (assert-true
  (equalp
   (delete-if-not #'(lambda (arg) (not (string-equal arg "love")))
                  (vector '("Love") '("LOve") '("LOVe") '("LOVE"))
                  :key #'car
                  :count 10
                  :from-end t
                  :start 1
                  :end 3)
   #(("Love") ("LOVE")))))
(define-test sacla-must-sequence.3660 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc"))
   "bcbc")))
(define-test sacla-must-sequence.3661 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (string-equal arg #\a))) (copy-seq ""))
   "")))
(define-test sacla-must-sequence.3662 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "xyz"))
   "xyz")))
(define-test sacla-must-sequence.3663 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "ABCABC")
                  :key #'char-downcase)
   "BCBC")))
(define-test sacla-must-sequence.3664 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count 1)
   "bcabc")))
(define-test sacla-must-sequence.3665 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count 1
                  :from-end t)
   "abcbc")))
(define-test sacla-must-sequence.3666 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count 0)
   "abcabc")))
(define-test sacla-must-sequence.3667 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count -10)
   "abcabc")))
(define-test sacla-must-sequence.3668 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count 0)
   "abcabc")))
(define-test sacla-must-sequence.3669 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc"))
   "bcbc")))
(define-test sacla-must-sequence.3670 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (string-equal arg #\b)))
                  (copy-seq "abcabc"))
   "acac")))
(define-test sacla-must-sequence.3671 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (string-equal arg #\c)))
                  (copy-seq "abcabc"))
   "abab")))
(define-test sacla-must-sequence.3672 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count 0)
   "abcabc")))
(define-test sacla-must-sequence.3673 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count 1)
   "bcabc")))
(define-test sacla-must-sequence.3674 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count 1
                  :from-end t)
   "abcbc")))
(define-test sacla-must-sequence.3675 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count 2)
   "bcbc")))
(define-test sacla-must-sequence.3676 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count 2
                  :from-end t)
   "bcbc")))
(define-test sacla-must-sequence.3677 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count 3)
   "bcbc")))
(define-test sacla-must-sequence.3678 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count 3
                  :from-end t)
   "bcbc")))
(define-test sacla-must-sequence.3679 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count 4)
   "bcbc")))
(define-test sacla-must-sequence.3680 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count 4
                  :from-end t)
   "bcbc")))
(define-test sacla-must-sequence.3681 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count -1)
   "abcabc")))
(define-test sacla-must-sequence.3682 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count -10)
   "abcabc")))
(define-test sacla-must-sequence.3683 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabc")
                  :count -100)
   "abcabc")))
(define-test sacla-must-sequence.3684 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabcabcabc")
                  :start 1)
   "abcbcbcbc")))
(define-test sacla-must-sequence.3685 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (string-equal arg #\a)))
                  (copy-seq "abcabcabcabc")
                  :start 1
                  :count 1)
   "abcbcabcabc")))
(define-test sacla-must-sequence.3686 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (eql arg #\a)))
                  (copy-seq "abcabcabcabc")
                  :start 1
                  :count 2)
   "abcbcbcabc")))
(define-test sacla-must-sequence.3687 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (eql arg #\a)))
                  (copy-seq "abcabcabcabc")
                  :start 1
                  :end nil
                  :count 2)
   "abcbcbcabc")))
(define-test sacla-must-sequence.3688 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (eql arg #\a)))
                  (copy-seq "abcabcabcabc")
                  :start 1
                  :end 8)
   "abcbcbcabc")))
(define-test sacla-must-sequence.3689 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (eql arg #\a)))
                  (copy-seq "abcabcabcabc")
                  :start 1
                  :end 8
                  :count 1)
   "abcbcabcabc")))
(define-test sacla-must-sequence.3690 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (eql arg #\a)))
                  (copy-seq "abcabcabcabc")
                  :start 1
                  :end 8
                  :count 1
                  :from-end t)
   "abcabcbcabc")))
(define-test sacla-must-sequence.3691 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (eql arg #\a)))
                  (copy-seq "abcabcabcabc")
                  :start 1
                  :end 8
                  :count 0
                  :from-end t)
   "abcabcabcabc")))
(define-test sacla-must-sequence.3692 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (eql arg #\a)))
                  (copy-seq "abcabcabcabc")
                  :start 1
                  :end 8
                  :count -100
                  :from-end t)
   "abcabcabcabc")))
(define-test sacla-must-sequence.3693 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (eql arg #\a)))
                  (copy-seq "abcabcabcabc")
                  :start 1
                  :end 1)
   "abcabcabcabc")))
(define-test sacla-must-sequence.3694 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (eql arg #\a)))
                  (copy-seq "abcabcabcabc")
                  :start 2
                  :end 2)
   "abcabcabcabc")))
(define-test sacla-must-sequence.3695 (:tag :sacla)
 (assert-true
  (string=
   (delete-if-not #'(lambda (arg) (not (eql arg #\a)))
                  (copy-seq "abcabcabcabc")
                  :start 12
                  :end 12)
   "abcabcabcabc")))
(define-test sacla-must-sequence.3696 (:tag :sacla)
 (assert-true (equal (delete-if-not (complement #'zerop) #*0101) #*11)))
(define-test sacla-must-sequence.3697 (:tag :sacla)
 (assert-true
  (equal (delete-if-not (complement #'zerop) #*01010101 :count 1) #*1010101)))
(define-test sacla-must-sequence.3698 (:tag :sacla)
 (assert-true
  (equal (delete-if-not (complement #'zerop) #*01010101 :count 1 :from-end t)
         #*0101011)))
(define-test sacla-must-sequence.3699 (:tag :sacla)
 (assert-true
  (equal (delete-if-not (complement #'zerop) #*01010101 :start 1) #*01111)))
(define-test sacla-must-sequence.3700 (:tag :sacla)
 (assert-true
  (equal (delete-if-not (complement #'zerop) #*01010101 :start 1 :end nil)
         #*01111)))
(define-test sacla-must-sequence.3701 (:tag :sacla)
 (assert-true
  (equal (delete-if-not (complement #'zerop) #*01010101 :start 1 :end 6)
         #*011101)))
(define-test sacla-must-sequence.3702 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not (complement #'zerop) #*01010101 :start 1 :end 6 :count 1)
   #*0110101)))
(define-test sacla-must-sequence.3703 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not (complement #'zerop)
                  #*01010101
                  :start 1
                  :end 6
                  :count 1
                  :from-end t)
   #*0101101)))
(define-test sacla-must-sequence.3704 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not (complement #'oddp)
                  #*01010101
                  :start 1
                  :end 6
                  :count 1
                  :from-end t)
   #*0101001)))
(define-test sacla-must-sequence.3705 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not (complement #'evenp)
                  #*01010101
                  :start 1
                  :end 6
                  :count 1
                  :from-end t)
   #*0101101)))
(define-test sacla-must-sequence.3706 (:tag :sacla)
 (assert-true
  (equal
   (delete-if-not (complement #'plusp)
                  #*01010101
                  :start 1
                  :end 6
                  :count 1
                  :from-end t
                  :key #'(lambda (arg) (* arg 10)))
   #*0101001)))
(define-test sacla-must-sequence.3707 (:tag :sacla)
 (assert-true
  (equal (remove-duplicates "aBcDAbCd" :test #'char-equal :from-end t) "aBcD")))
(define-test sacla-must-sequence.3708 (:tag :sacla)
 (assert-true (equal (remove-duplicates '(a b c b d d e)) '(a c b d e))))
(define-test sacla-must-sequence.3709 (:tag :sacla)
 (assert-true
  (equal (remove-duplicates '(a b c b d d e) :from-end t) '(a b c d e))))
(define-test sacla-must-sequence.3710 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates '((foo #\a) (bar #\%) (baz #\A))
                      :test #'char-equal
                      :key #'cadr)
   '((bar #\%) (baz #\A)))))
(define-test sacla-must-sequence.3711 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates '((foo #\a) (bar #\%) (baz #\A))
                      :test #'char-equal
                      :key #'cadr
                      :from-end t)
   '((foo #\a) (bar #\%)))))
(define-test sacla-must-sequence.3712 (:tag :sacla)
 (assert-true
  (let* ((list0 (list 0 1 2 3 4 5 6))
         (list (delete-duplicates list0 :key #'oddp :start 1 :end 6)))
    (equal list '(0 4 5 6)))))
(define-test sacla-must-sequence.3713 (:tag :sacla)
 (assert-true
  (let* ((list0 (list 0 1 2)) (list (remove-duplicates list0)))
    (and (not (eq list0 list)) (equal list0 '(0 1 2)) (equal list '(0 1 2))))))
(define-test sacla-must-sequence.3714 (:tag :sacla)
 (assert-true
  (let* ((list0 (list 2 1 0 1 0 1 2)) (list (remove-duplicates list0)))
    (and (not (eq list0 list))
         (equal list0 '(2 1 0 1 0 1 2))
         (equal list '(0 1 2))))))
(define-test sacla-must-sequence.3715 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 0 1 2)) (vector (remove-duplicates vector0)))
    (and (not (eq vector0 vector))
         (equalp vector0 #(0 1 2))
         (equalp vector #(0 1 2))))))
(define-test sacla-must-sequence.3716 (:tag :sacla)
 (assert-true
  (let* ((vector0 (vector 2 1 0 1 0 1 2)) (vector (remove-duplicates vector0)))
    (and (not (eq vector0 vector))
         (equalp vector0 #(2 1 0 1 0 1 2))
         (equalp vector #(0 1 2))))))
(define-test sacla-must-sequence.3717 (:tag :sacla)
 (assert-true (equal (remove-duplicates (list 0 1 2 2 3 3 3)) '(0 1 2 3))))
(define-test sacla-must-sequence.3718 (:tag :sacla)
 (assert-true
  (equal (remove-duplicates (list 0 0 0 2 0 1 1 2 2 2 1 1 1 1 2)) '(0 1 2))))
(define-test sacla-must-sequence.3719 (:tag :sacla)
 (assert-true (equal (remove-duplicates (list 'a 'a 'b 'c 'c)) '(a b c))))
(define-test sacla-must-sequence.3720 (:tag :sacla)
 (assert-true (equal (remove-duplicates (list 0 1 2)) '(0 1 2))))
(define-test sacla-must-sequence.3721 (:tag :sacla)
 (assert-true
  (equal (remove-duplicates (list 2 0 2 1 1 1 0 0 0 1 2)) '(0 1 2))))
(define-test sacla-must-sequence.3722 (:tag :sacla)
 (assert-true (equal (remove-duplicates (list)) 'nil)))
(define-test sacla-must-sequence.3723 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates
    (list '(x . 0) '(y . 1) '(z . 2) '(a . 0) '(b . 1) '(c . 2))
    :key #'cdr)
   '((a . 0) (b . 1) (c . 2)))))
(define-test sacla-must-sequence.3724 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates
    (list '(x . 0) '(y . 1) '(z . 2) '(a . 0) '(b . 1) '(c . 2))
    :key #'cdr
    :test #'(lambda (a b) (and (oddp a) (oddp b))))
   '((x . 0) (z . 2) (a . 0) (b . 1) (c . 2)))))
(define-test sacla-must-sequence.3725 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates
    (list '(x . 0) '(y . 1) '(z . 2) '(a . 0) '(b . 1) '(c . 2))
    :key #'cdr
    :test-not #'(lambda (a b) (not (and (oddp a) (oddp b)))))
   '((x . 0) (z . 2) (a . 0) (b . 1) (c . 2)))))
(define-test sacla-must-sequence.3726 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates
    (list '(x . 0) '(y . 1) '(z . 2) '(a . 0) '(b . 1) '(c . 2))
    :key #'cdr
    :test #'(lambda (a b) (and (evenp a) (evenp b))))
   '((y . 1) (b . 1) (c . 2)))))
(define-test sacla-must-sequence.3727 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates
    (list '(x . 0) '(y . 1) '(z . 2) '(a . 0) '(b . 1) '(c . 2))
    :key #'cdr
    :test-not #'(lambda (a b) (not (and (evenp a) (evenp b)))))
   '((y . 1) (b . 1) (c . 2)))))
(define-test sacla-must-sequence.3728 (:tag :sacla)
 (assert-true
  (equal (remove-duplicates (list 0 1 2 0 1 2 0 1 2 0 1 2) :start 3 :end 9)
         '(0 1 2 0 1 2 0 1 2))))
(define-test sacla-must-sequence.3729 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates
    (list '(0 . 0)
          '(1 . 1)
          '(2 . 2)
          '(0 . 3)
          '(1 . 4)
          '(2 . 5)
          '(0 . 6)
          '(1 . 7)
          '(2 . 8)
          '(0 . 9)
          '(1 . 10)
          '(2 . 11)))
   (list '(0 . 0)
         '(1 . 1)
         '(2 . 2)
         '(0 . 3)
         '(1 . 4)
         '(2 . 5)
         '(0 . 6)
         '(1 . 7)
         '(2 . 8)
         '(0 . 9)
         '(1 . 10)
         '(2 . 11)))))
(define-test sacla-must-sequence.3730 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates
    (list '(0 . 0)
          '(1 . 1)
          '(2 . 2)
          '(0 . 3)
          '(1 . 4)
          '(2 . 5)
          '(0 . 6)
          '(1 . 7)
          '(2 . 8)
          '(0 . 9)
          '(1 . 10)
          '(2 . 11))
    :key #'car)
   '((0 . 9) (1 . 10) (2 . 11)))))
(define-test sacla-must-sequence.3731 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates
    (list '(0 . 0)
          '(1 . 1)
          '(2 . 2)
          '(0 . 3)
          '(1 . 4)
          '(2 . 5)
          '(0 . 6)
          '(1 . 7)
          '(2 . 8)
          '(0 . 9)
          '(1 . 10)
          '(2 . 11))
    :key #'car
    :from-end t)
   (list '(0 . 0) '(1 . 1) '(2 . 2)))))
(define-test sacla-must-sequence.3732 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates
    (list '(0 . 0)
          '(1 . 1)
          '(2 . 2)
          '(0 . 3)
          '(1 . 4)
          '(2 . 5)
          '(0 . 6)
          '(1 . 7)
          '(2 . 8)
          '(0 . 9)
          '(1 . 10)
          '(2 . 11))
    :start 3
    :key #'car)
   (list '(0 . 0) '(1 . 1) '(2 . 2) '(0 . 9) '(1 . 10) '(2 . 11)))))
(define-test sacla-must-sequence.3733 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates
    (list '(0 . 0)
          '(1 . 1)
          '(2 . 2)
          '(0 . 3)
          '(1 . 4)
          '(2 . 5)
          '(0 . 6)
          '(1 . 7)
          '(2 . 8)
          '(0 . 9)
          '(1 . 10)
          '(2 . 11))
    :start 3
    :key #'car
    :from-end t)
   (list '(0 . 0) '(1 . 1) '(2 . 2) '(0 . 3) '(1 . 4) '(2 . 5)))))
(define-test sacla-must-sequence.3734 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates
    (list '(0 . 0)
          '(1 . 1)
          '(2 . 2)
          '(0 . 3)
          '(1 . 4)
          '(2 . 5)
          '(0 . 6)
          '(1 . 7)
          '(2 . 8)
          '(0 . 9)
          '(1 . 10)
          '(2 . 11))
    :start 3
    :end nil
    :key #'car)
   (list '(0 . 0) '(1 . 1) '(2 . 2) '(0 . 9) '(1 . 10) '(2 . 11)))))
(define-test sacla-must-sequence.3735 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates
    (list '(0 . 0)
          '(1 . 1)
          '(2 . 2)
          '(0 . 3)
          '(1 . 4)
          '(2 . 5)
          '(0 . 6)
          '(1 . 7)
          '(2 . 8)
          '(0 . 9)
          '(1 . 10)
          '(2 . 11))
    :start 3
    :end 9
    :key #'car)
   '((0 . 0) (1 . 1) (2 . 2) (0 . 6) (1 . 7) (2 . 8) (0 . 9) (1 . 10)
     (2 . 11)))))
(define-test sacla-must-sequence.3736 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates
    (list '(0 . 0)
          '(1 . 1)
          '(2 . 2)
          '(0 . 3)
          '(1 . 4)
          '(2 . 5)
          '(0 . 6)
          '(1 . 7)
          '(2 . 8)
          '(0 . 9)
          '(1 . 10)
          '(2 . 11))
    :start 3
    :end 9
    :key #'car
    :from-end t)
   (list '(0 . 0)
         '(1 . 1)
         '(2 . 2)
         '(0 . 3)
         '(1 . 4)
         '(2 . 5)
         '(0 . 9)
         '(1 . 10)
         '(2 . 11)))))
(define-test sacla-must-sequence.3737 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates
    (list "Monday"
          "Tuesday"
          "Wednesday"
          "Thursday"
          "Friday"
          "Saturday"
          "Sunday")
    :key #'length)
   (list "Tuesday" "Wednesday" "Saturday" "Sunday"))))
(define-test sacla-must-sequence.3738 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates
    (list "Monday"
          "Tuesday"
          "Wednesday"
          "Thursday"
          "Friday"
          "Saturday"
          "Sunday")
    :key #'(lambda (arg) (char arg 0))
    :test #'char=)
   (list "Monday" "Wednesday" "Thursday" "Friday" "Sunday"))))
(define-test sacla-must-sequence.3739 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates
    (list "Monday"
          "Tuesday"
          "Wednesday"
          "Thursday"
          "Friday"
          "Saturday"
          "Sunday")
    :key #'(lambda (arg) (char arg 0))
    :test-not (complement #'char=))
   (list "Monday" "Wednesday" "Thursday" "Friday" "Sunday"))))
(define-test sacla-must-sequence.3740 (:tag :sacla)
 (assert-true
  (equal (remove-duplicates (list #\a #\b #\c #\A #\B #\C) :key #'char-upcase)
         '(#\A #\B #\C))))
(define-test sacla-must-sequence.3741 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates (list #\a #\b #\c #\A #\B #\C)
                      :key #'char-upcase
                      :from-end t)
   '(#\a #\b #\c))))
(define-test sacla-must-sequence.3742 (:tag :sacla)
 (assert-true
  (equal (remove-duplicates (list #\a #\b #\c #\A #\B #\C) :test #'char=)
         (list #\a #\b #\c #\A #\B #\C))))
(define-test sacla-must-sequence.3743 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates (list #\a #\b #\c #\A #\B #\C)
                      :test-not (complement #'char=))
   (list #\a #\b #\c #\A #\B #\C))))
(define-test sacla-must-sequence.3744 (:tag :sacla)
 (assert-true
  (equal (remove-duplicates (list #\a #\b #\c #\A #\B #\C) :test #'char-equal)
         (list #\A #\B #\C))))
(define-test sacla-must-sequence.3745 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates (list #\a #\b #\c #\A #\B #\C)
                      :test-not (complement #'char-equal))
   (list #\A #\B #\C))))
(define-test sacla-must-sequence.3746 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates (list #\a #\b #\c #\A #\B #\C)
                      :test #'char-equal
                      :from-end t)
   (list #\a #\b #\c))))
(define-test sacla-must-sequence.3747 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates (list #\a #\b #\c #\A #\B #\C)
                      :test-not (complement #'char-equal)
                      :from-end t)
   (list #\a #\b #\c))))
(define-test sacla-must-sequence.3748 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates (list #\a #\1 #\b #\1 #\2 #\c #\0 #\A #\0 #\B #\C #\9)
                      :key #'alpha-char-p)
   (list #\C #\9))))
(define-test sacla-must-sequence.3749 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates (list #\a #\1 #\b #\1 #\2 #\c #\0 #\A #\0 #\B #\C #\9)
                      :key #'alphanumericp)
   (list #\9))))
(define-test sacla-must-sequence.3750 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates
    (list '(#\A . 0)
          '(#\b . 1)
          '(#\C . 2)
          '(#\a . 3)
          '(#\B . 4)
          '(#\c . 5)
          '(#\A . 6)
          '(#\b . 7)
          '(#\C . 8)
          '(#\a . 9)
          '(#\B . 10)
          '(#\c . 11)))
   (list '(#\A . 0)
         '(#\b . 1)
         '(#\C . 2)
         '(#\a . 3)
         '(#\B . 4)
         '(#\c . 5)
         '(#\A . 6)
         '(#\b . 7)
         '(#\C . 8)
         '(#\a . 9)
         '(#\B . 10)
         '(#\c . 11)))))
(define-test sacla-must-sequence.3751 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates
    (list '(#\A . 0)
          '(#\b . 1)
          '(#\C . 2)
          '(#\a . 3)
          '(#\B . 4)
          '(#\c . 5)
          '(#\A . 6)
          '(#\b . 7)
          '(#\C . 8)
          '(#\a . 9)
          '(#\B . 10)
          '(#\c . 11))
    :key #'car)
   (list '(#\A . 6) '(#\b . 7) '(#\C . 8) '(#\a . 9) '(#\B . 10) '(#\c . 11)))))
(define-test sacla-must-sequence.3752 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates
    (list '(#\A . 0)
          '(#\b . 1)
          '(#\C . 2)
          '(#\a . 3)
          '(#\B . 4)
          '(#\c . 5)
          '(#\A . 6)
          '(#\b . 7)
          '(#\C . 8)
          '(#\a . 9)
          '(#\B . 10)
          '(#\c . 11))
    :key #'car
    :start 3
    :end 9)
   (list '(#\A . 0)
         '(#\b . 1)
         '(#\C . 2)
         '(#\a . 3)
         '(#\B . 4)
         '(#\c . 5)
         '(#\A . 6)
         '(#\b . 7)
         '(#\C . 8)
         '(#\a . 9)
         '(#\B . 10)
         '(#\c . 11)))))
(define-test sacla-must-sequence.3753 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates
    (list '(#\A . 0)
          '(#\b . 1)
          '(#\C . 2)
          '(#\a . 3)
          '(#\B . 4)
          '(#\c . 5)
          '(#\A . 6)
          '(#\b . 7)
          '(#\C . 8)
          '(#\a . 9)
          '(#\B . 10)
          '(#\c . 11))
    :key #'car
    :start 3
    :end 9
    :test #'char-equal)
   (list '(#\A . 0)
         '(#\b . 1)
         '(#\C . 2)
         '(#\A . 6)
         '(#\b . 7)
         '(#\C . 8)
         '(#\a . 9)
         '(#\B . 10)
         '(#\c . 11)))))
(define-test sacla-must-sequence.3754 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates
    (list '(#\A . 0)
          '(#\b . 1)
          '(#\C . 2)
          '(#\a . 3)
          '(#\B . 4)
          '(#\c . 5)
          '(#\A . 6)
          '(#\b . 7)
          '(#\C . 8)
          '(#\a . 9)
          '(#\B . 10)
          '(#\c . 11))
    :key #'car
    :start 3
    :end 9
    :test-not (complement #'char-equal))
   (list '(#\A . 0)
         '(#\b . 1)
         '(#\C . 2)
         '(#\A . 6)
         '(#\b . 7)
         '(#\C . 8)
         '(#\a . 9)
         '(#\B . 10)
         '(#\c . 11)))))
(define-test sacla-must-sequence.3755 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates
    (list '(#\A . 0)
          '(#\b . 1)
          '(#\C . 2)
          '(#\a . 3)
          '(#\B . 4)
          '(#\c . 5)
          '(#\A . 6)
          '(#\b . 7)
          '(#\C . 8)
          '(#\a . 9)
          '(#\B . 10)
          '(#\c . 11))
    :key #'car
    :start 3
    :end 9
    :test #'char-equal
    :from-end t)
   (list '(#\A . 0)
         '(#\b . 1)
         '(#\C . 2)
         '(#\a . 3)
         '(#\B . 4)
         '(#\c . 5)
         '(#\a . 9)
         '(#\B . 10)
         '(#\c . 11)))))
(define-test sacla-must-sequence.3756 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates
    (list '(#\A . 0)
          '(#\b . 1)
          '(#\C . 2)
          '(#\a . 3)
          '(#\B . 4)
          '(#\c . 5)
          '(#\A . 6)
          '(#\b . 7)
          '(#\C . 8)
          '(#\a . 9)
          '(#\B . 10)
          '(#\c . 11))
    :key #'car
    :start 3
    :end 9
    :test-not (complement #'char-equal)
    :from-end t)
   (list '(#\A . 0)
         '(#\b . 1)
         '(#\C . 2)
         '(#\a . 3)
         '(#\B . 4)
         '(#\c . 5)
         '(#\a . 9)
         '(#\B . 10)
         '(#\c . 11)))))
(define-test sacla-must-sequence.3757 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates
    (list '(#\A . 0)
          '(#\b . 1)
          '(#\C . 2)
          '(#\a . 3)
          '(#\B . 4)
          '(#\c . 5)
          '(#\A . 6)
          '(#\b . 7)
          '(#\C . 8)
          '(#\a . 9)
          '(#\B . 10)
          '(#\c . 11))
    :key #'car
    :start 3)
   (list '(#\A . 0)
         '(#\b . 1)
         '(#\C . 2)
         '(#\A . 6)
         '(#\b . 7)
         '(#\C . 8)
         '(#\a . 9)
         '(#\B . 10)
         '(#\c . 11)))))
(define-test sacla-must-sequence.3758 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates
    (list '(#\A . 0)
          '(#\b . 1)
          '(#\C . 2)
          '(#\a . 3)
          '(#\B . 4)
          '(#\c . 5)
          '(#\A . 6)
          '(#\b . 7)
          '(#\C . 8)
          '(#\a . 9)
          '(#\B . 10)
          '(#\c . 11))
    :key #'car
    :start 3
    :end nil)
   (list '(#\A . 0)
         '(#\b . 1)
         '(#\C . 2)
         '(#\A . 6)
         '(#\b . 7)
         '(#\C . 8)
         '(#\a . 9)
         '(#\B . 10)
         '(#\c . 11)))))
(define-test sacla-must-sequence.3759 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates
    (list '(#\A . 0)
          '(#\b . 1)
          '(#\C . 2)
          '(#\a . 3)
          '(#\B . 4)
          '(#\c . 5)
          '(#\A . 6)
          '(#\b . 7)
          '(#\C . 8)
          '(#\a . 9)
          '(#\B . 10)
          '(#\c . 11))
    :key #'car
    :start 3
    :from-end t)
   (list '(#\A . 0)
         '(#\b . 1)
         '(#\C . 2)
         '(#\a . 3)
         '(#\B . 4)
         '(#\c . 5)
         '(#\A . 6)
         '(#\b . 7)
         '(#\C . 8)))))
(define-test sacla-must-sequence.3760 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates
    (list '(#\A . 0)
          '(#\b . 1)
          '(#\C . 2)
          '(#\a . 3)
          '(#\B . 4)
          '(#\c . 5)
          '(#\A . 6)
          '(#\b . 7)
          '(#\C . 8)
          '(#\a . 9)
          '(#\B . 10)
          '(#\c . 11))
    :key #'car
    :end 9)
   (list '(#\a . 3)
         '(#\B . 4)
         '(#\c . 5)
         '(#\A . 6)
         '(#\b . 7)
         '(#\C . 8)
         '(#\a . 9)
         '(#\B . 10)
         '(#\c . 11)))))
(define-test sacla-must-sequence.3761 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates
    (list '(#\A . 0)
          '(#\b . 1)
          '(#\C . 2)
          '(#\a . 3)
          '(#\B . 4)
          '(#\c . 5)
          '(#\A . 6)
          '(#\b . 7)
          '(#\C . 8)
          '(#\a . 9)
          '(#\B . 10)
          '(#\c . 11))
    :key #'car
    :end 9
    :from-end t)
   (list '(#\A . 0)
         '(#\b . 1)
         '(#\C . 2)
         '(#\a . 3)
         '(#\B . 4)
         '(#\c . 5)
         '(#\a . 9)
         '(#\B . 10)
         '(#\c . 11)))))
(define-test sacla-must-sequence.3762 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates
    (list '(#\A . 0)
          '(#\b . 1)
          '(#\C . 2)
          '(#\a . 3)
          '(#\B . 4)
          '(#\c . 5)
          '(#\A . 6)
          '(#\b . 7)
          '(#\C . 8)
          '(#\a . 9)
          '(#\B . 10)
          '(#\c . 11))
    :key #'car
    :start 0
    :end 12
    :test #'char-equal)
   (list '(#\a . 9) '(#\B . 10) '(#\c . 11)))))
(define-test sacla-must-sequence.3763 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates
    (list '(#\A . 0)
          '(#\b . 1)
          '(#\C . 2)
          '(#\a . 3)
          '(#\B . 4)
          '(#\c . 5)
          '(#\A . 6)
          '(#\b . 7)
          '(#\C . 8)
          '(#\a . 9)
          '(#\B . 10)
          '(#\c . 11))
    :key #'car
    :start 0
    :end 12
    :test-not (complement #'char-equal))
   (list '(#\a . 9) '(#\B . 10) '(#\c . 11)))))
(define-test sacla-must-sequence.3764 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates
    (list '(#\A . 0)
          '(#\b . 1)
          '(#\C . 2)
          '(#\a . 3)
          '(#\B . 4)
          '(#\c . 5)
          '(#\A . 6)
          '(#\b . 7)
          '(#\C . 8)
          '(#\a . 9)
          '(#\B . 10)
          '(#\c . 11))
    :key #'car
    :start 0
    :end 12
    :test #'char-equal
    :from-end t)
   '((#\A . 0) (#\b . 1) (#\C . 2)))))
(define-test sacla-must-sequence.3765 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates
    (list '(#\A . 0)
          '(#\b . 1)
          '(#\C . 2)
          '(#\a . 3)
          '(#\B . 4)
          '(#\c . 5)
          '(#\A . 6)
          '(#\b . 7)
          '(#\C . 8)
          '(#\a . 9)
          '(#\B . 10)
          '(#\c . 11))
    :key #'car
    :start 0
    :end 12
    :test-not (complement #'char-equal)
    :from-end t)
   '((#\A . 0) (#\b . 1) (#\C . 2)))))
(define-test sacla-must-sequence.3766 (:tag :sacla)
 (assert-true (equalp (remove-duplicates (vector 0 1 2 2 3 3 3)) #(0 1 2 3))))
(define-test sacla-must-sequence.3767 (:tag :sacla)
 (assert-true
  (equalp (remove-duplicates (vector 0 0 0 2 0 1 1 2 2 2 1 1 1 1 2)) #(0 1 2))))
(define-test sacla-must-sequence.3768 (:tag :sacla)
 (assert-true (equalp (remove-duplicates (vector 'a 'a 'b 'c 'c)) #(a b c))))
(define-test sacla-must-sequence.3769 (:tag :sacla)
 (assert-true (equalp (remove-duplicates (vector 0 1 2)) #(0 1 2))))
(define-test sacla-must-sequence.3770 (:tag :sacla)
 (assert-true
  (equalp (remove-duplicates (vector 2 0 2 1 1 1 0 0 0 1 2)) #(0 1 2))))
(define-test sacla-must-sequence.3771 (:tag :sacla)
 (assert-true (equalp (remove-duplicates (vector)) #())))
(define-test sacla-must-sequence.3772 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates
    (vector '(x . 0) '(y . 1) '(z . 2) '(a . 0) '(b . 1) '(c . 2))
    :key #'cdr)
   #((a . 0) (b . 1) (c . 2)))))
(define-test sacla-must-sequence.3773 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates
    (vector '(x . 0) '(y . 1) '(z . 2) '(a . 0) '(b . 1) '(c . 2))
    :key #'cdr
    :test #'(lambda (a b) (and (oddp a) (oddp b))))
   #((x . 0) (z . 2) (a . 0) (b . 1) (c . 2)))))
(define-test sacla-must-sequence.3774 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates
    (vector '(x . 0) '(y . 1) '(z . 2) '(a . 0) '(b . 1) '(c . 2))
    :key #'cdr
    :test-not #'(lambda (a b) (not (and (oddp a) (oddp b)))))
   #((x . 0) (z . 2) (a . 0) (b . 1) (c . 2)))))
(define-test sacla-must-sequence.3775 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates
    (vector '(x . 0) '(y . 1) '(z . 2) '(a . 0) '(b . 1) '(c . 2))
    :key #'cdr
    :test #'(lambda (a b) (and (evenp a) (evenp b))))
   #((y . 1) (b . 1) (c . 2)))))
(define-test sacla-must-sequence.3776 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates
    (vector '(x . 0) '(y . 1) '(z . 2) '(a . 0) '(b . 1) '(c . 2))
    :key #'cdr
    :test-not #'(lambda (a b) (not (and (evenp a) (evenp b)))))
   #((y . 1) (b . 1) (c . 2)))))
(define-test sacla-must-sequence.3777 (:tag :sacla)
 (assert-true
  (equalp (remove-duplicates (vector 0 1 2 0 1 2 0 1 2 0 1 2) :start 3 :end 9)
          #(0 1 2 0 1 2 0 1 2))))
(define-test sacla-must-sequence.3778 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates
    (vector '(0 . 0)
            '(1 . 1)
            '(2 . 2)
            '(0 . 3)
            '(1 . 4)
            '(2 . 5)
            '(0 . 6)
            '(1 . 7)
            '(2 . 8)
            '(0 . 9)
            '(1 . 10)
            '(2 . 11)))
   (vector '(0 . 0)
           '(1 . 1)
           '(2 . 2)
           '(0 . 3)
           '(1 . 4)
           '(2 . 5)
           '(0 . 6)
           '(1 . 7)
           '(2 . 8)
           '(0 . 9)
           '(1 . 10)
           '(2 . 11)))))
(define-test sacla-must-sequence.3779 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates
    (vector '(0 . 0)
            '(1 . 1)
            '(2 . 2)
            '(0 . 3)
            '(1 . 4)
            '(2 . 5)
            '(0 . 6)
            '(1 . 7)
            '(2 . 8)
            '(0 . 9)
            '(1 . 10)
            '(2 . 11))
    :key #'car)
   #((0 . 9) (1 . 10) (2 . 11)))))
(define-test sacla-must-sequence.3780 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates
    (vector '(0 . 0)
            '(1 . 1)
            '(2 . 2)
            '(0 . 3)
            '(1 . 4)
            '(2 . 5)
            '(0 . 6)
            '(1 . 7)
            '(2 . 8)
            '(0 . 9)
            '(1 . 10)
            '(2 . 11))
    :key #'car
    :from-end t)
   (vector '(0 . 0) '(1 . 1) '(2 . 2)))))
(define-test sacla-must-sequence.3781 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates
    (vector '(0 . 0)
            '(1 . 1)
            '(2 . 2)
            '(0 . 3)
            '(1 . 4)
            '(2 . 5)
            '(0 . 6)
            '(1 . 7)
            '(2 . 8)
            '(0 . 9)
            '(1 . 10)
            '(2 . 11))
    :start 3
    :key #'car)
   (vector '(0 . 0) '(1 . 1) '(2 . 2) '(0 . 9) '(1 . 10) '(2 . 11)))))
(define-test sacla-must-sequence.3782 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates
    (vector '(0 . 0)
            '(1 . 1)
            '(2 . 2)
            '(0 . 3)
            '(1 . 4)
            '(2 . 5)
            '(0 . 6)
            '(1 . 7)
            '(2 . 8)
            '(0 . 9)
            '(1 . 10)
            '(2 . 11))
    :start 3
    :key #'car
    :from-end t)
   (vector '(0 . 0) '(1 . 1) '(2 . 2) '(0 . 3) '(1 . 4) '(2 . 5)))))
(define-test sacla-must-sequence.3783 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates
    (vector '(0 . 0)
            '(1 . 1)
            '(2 . 2)
            '(0 . 3)
            '(1 . 4)
            '(2 . 5)
            '(0 . 6)
            '(1 . 7)
            '(2 . 8)
            '(0 . 9)
            '(1 . 10)
            '(2 . 11))
    :start 3
    :end nil
    :key #'car)
   (vector '(0 . 0) '(1 . 1) '(2 . 2) '(0 . 9) '(1 . 10) '(2 . 11)))))
(define-test sacla-must-sequence.3784 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates
    (vector '(0 . 0)
            '(1 . 1)
            '(2 . 2)
            '(0 . 3)
            '(1 . 4)
            '(2 . 5)
            '(0 . 6)
            '(1 . 7)
            '(2 . 8)
            '(0 . 9)
            '(1 . 10)
            '(2 . 11))
    :start 3
    :end 9
    :key #'car)
   #((0 . 0) (1 . 1) (2 . 2) (0 . 6) (1 . 7) (2 . 8) (0 . 9) (1 . 10)
     (2 . 11)))))
(define-test sacla-must-sequence.3785 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates
    (vector '(0 . 0)
            '(1 . 1)
            '(2 . 2)
            '(0 . 3)
            '(1 . 4)
            '(2 . 5)
            '(0 . 6)
            '(1 . 7)
            '(2 . 8)
            '(0 . 9)
            '(1 . 10)
            '(2 . 11))
    :start 3
    :end 9
    :key #'car
    :from-end t)
   (vector '(0 . 0)
           '(1 . 1)
           '(2 . 2)
           '(0 . 3)
           '(1 . 4)
           '(2 . 5)
           '(0 . 9)
           '(1 . 10)
           '(2 . 11)))))
(define-test sacla-must-sequence.3786 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates
    (vector "Monday"
            "Tuesday"
            "Wednesday"
            "Thursday"
            "Friday"
            "Saturday"
            "Sunday")
    :key #'length)
   (vector "Tuesday" "Wednesday" "Saturday" "Sunday"))))
(define-test sacla-must-sequence.3787 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates
    (vector "Monday"
            "Tuesday"
            "Wednesday"
            "Thursday"
            "Friday"
            "Saturday"
            "Sunday")
    :key #'(lambda (arg) (char arg 0))
    :test #'char=)
   (vector "Monday" "Wednesday" "Thursday" "Friday" "Sunday"))))
(define-test sacla-must-sequence.3788 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates
    (vector "Monday"
            "Tuesday"
            "Wednesday"
            "Thursday"
            "Friday"
            "Saturday"
            "Sunday")
    :key #'(lambda (arg) (char arg 0))
    :test-not (complement #'char=))
   (vector "Monday" "Wednesday" "Thursday" "Friday" "Sunday"))))
(define-test sacla-must-sequence.3789 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates (vector #\a #\b #\c #\A #\B #\C) :key #'char-upcase)
   #(#\A #\B #\C))))
(define-test sacla-must-sequence.3790 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates (vector #\a #\b #\c #\A #\B #\C)
                      :key #'char-upcase
                      :from-end t)
   #(#\a #\b #\c))))
(define-test sacla-must-sequence.3791 (:tag :sacla)
 (assert-true
  (equalp (remove-duplicates (vector #\a #\b #\c #\A #\B #\C) :test #'char=)
          (vector #\a #\b #\c #\A #\B #\C))))
(define-test sacla-must-sequence.3792 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates (vector #\a #\b #\c #\A #\B #\C)
                      :test-not (complement #'char=))
   (vector #\a #\b #\c #\A #\B #\C))))
(define-test sacla-must-sequence.3793 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates (vector #\a #\b #\c #\A #\B #\C) :test #'char-equal)
   (vector #\A #\B #\C))))
(define-test sacla-must-sequence.3794 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates (vector #\a #\b #\c #\A #\B #\C)
                      :test-not (complement #'char-equal))
   (vector #\A #\B #\C))))
(define-test sacla-must-sequence.3795 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates (vector #\a #\b #\c #\A #\B #\C)
                      :test #'char-equal
                      :from-end t)
   (vector #\a #\b #\c))))
(define-test sacla-must-sequence.3796 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates (vector #\a #\b #\c #\A #\B #\C)
                      :test-not (complement #'char-equal)
                      :from-end t)
   (vector #\a #\b #\c))))
(define-test sacla-must-sequence.3797 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates (vector #\a #\1 #\b #\1 #\2 #\c #\0 #\A #\0 #\B #\C #\9)
                      :key #'alpha-char-p)
   (vector #\C #\9))))
(define-test sacla-must-sequence.3798 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates (vector #\a #\1 #\b #\1 #\2 #\c #\0 #\A #\0 #\B #\C #\9)
                      :key #'alphanumericp)
   (vector #\9))))
(define-test sacla-must-sequence.3799 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates
    (vector '(#\A . 0)
            '(#\b . 1)
            '(#\C . 2)
            '(#\a . 3)
            '(#\B . 4)
            '(#\c . 5)
            '(#\A . 6)
            '(#\b . 7)
            '(#\C . 8)
            '(#\a . 9)
            '(#\B . 10)
            '(#\c . 11)))
   (vector '(#\A . 0)
           '(#\b . 1)
           '(#\C . 2)
           '(#\a . 3)
           '(#\B . 4)
           '(#\c . 5)
           '(#\A . 6)
           '(#\b . 7)
           '(#\C . 8)
           '(#\a . 9)
           '(#\B . 10)
           '(#\c . 11)))))
(define-test sacla-must-sequence.3800 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates
    (vector '(#\A . 0)
            '(#\b . 1)
            '(#\C . 2)
            '(#\a . 3)
            '(#\B . 4)
            '(#\c . 5)
            '(#\A . 6)
            '(#\b . 7)
            '(#\C . 8)
            '(#\a . 9)
            '(#\B . 10)
            '(#\c . 11))
    :key #'car)
   (vector '(#\A . 6)
           '(#\b . 7)
           '(#\C . 8)
           '(#\a . 9)
           '(#\B . 10)
           '(#\c . 11)))))
(define-test sacla-must-sequence.3801 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates
    (vector '(#\A . 0)
            '(#\b . 1)
            '(#\C . 2)
            '(#\a . 3)
            '(#\B . 4)
            '(#\c . 5)
            '(#\A . 6)
            '(#\b . 7)
            '(#\C . 8)
            '(#\a . 9)
            '(#\B . 10)
            '(#\c . 11))
    :key #'car
    :start 3
    :end 9)
   (vector '(#\A . 0)
           '(#\b . 1)
           '(#\C . 2)
           '(#\a . 3)
           '(#\B . 4)
           '(#\c . 5)
           '(#\A . 6)
           '(#\b . 7)
           '(#\C . 8)
           '(#\a . 9)
           '(#\B . 10)
           '(#\c . 11)))))
(define-test sacla-must-sequence.3802 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates
    (vector '(#\A . 0)
            '(#\b . 1)
            '(#\C . 2)
            '(#\a . 3)
            '(#\B . 4)
            '(#\c . 5)
            '(#\A . 6)
            '(#\b . 7)
            '(#\C . 8)
            '(#\a . 9)
            '(#\B . 10)
            '(#\c . 11))
    :key #'car
    :start 3
    :end 9
    :test #'char-equal)
   (vector '(#\A . 0)
           '(#\b . 1)
           '(#\C . 2)
           '(#\A . 6)
           '(#\b . 7)
           '(#\C . 8)
           '(#\a . 9)
           '(#\B . 10)
           '(#\c . 11)))))
(define-test sacla-must-sequence.3803 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates
    (vector '(#\A . 0)
            '(#\b . 1)
            '(#\C . 2)
            '(#\a . 3)
            '(#\B . 4)
            '(#\c . 5)
            '(#\A . 6)
            '(#\b . 7)
            '(#\C . 8)
            '(#\a . 9)
            '(#\B . 10)
            '(#\c . 11))
    :key #'car
    :start 3
    :end 9
    :test-not (complement #'char-equal))
   (vector '(#\A . 0)
           '(#\b . 1)
           '(#\C . 2)
           '(#\A . 6)
           '(#\b . 7)
           '(#\C . 8)
           '(#\a . 9)
           '(#\B . 10)
           '(#\c . 11)))))
(define-test sacla-must-sequence.3804 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates
    (vector '(#\A . 0)
            '(#\b . 1)
            '(#\C . 2)
            '(#\a . 3)
            '(#\B . 4)
            '(#\c . 5)
            '(#\A . 6)
            '(#\b . 7)
            '(#\C . 8)
            '(#\a . 9)
            '(#\B . 10)
            '(#\c . 11))
    :key #'car
    :start 3
    :end 9
    :test #'char-equal
    :from-end t)
   (vector '(#\A . 0)
           '(#\b . 1)
           '(#\C . 2)
           '(#\a . 3)
           '(#\B . 4)
           '(#\c . 5)
           '(#\a . 9)
           '(#\B . 10)
           '(#\c . 11)))))
(define-test sacla-must-sequence.3805 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates
    (vector '(#\A . 0)
            '(#\b . 1)
            '(#\C . 2)
            '(#\a . 3)
            '(#\B . 4)
            '(#\c . 5)
            '(#\A . 6)
            '(#\b . 7)
            '(#\C . 8)
            '(#\a . 9)
            '(#\B . 10)
            '(#\c . 11))
    :key #'car
    :start 3
    :end 9
    :test-not (complement #'char-equal)
    :from-end t)
   (vector '(#\A . 0)
           '(#\b . 1)
           '(#\C . 2)
           '(#\a . 3)
           '(#\B . 4)
           '(#\c . 5)
           '(#\a . 9)
           '(#\B . 10)
           '(#\c . 11)))))
(define-test sacla-must-sequence.3806 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates
    (vector '(#\A . 0)
            '(#\b . 1)
            '(#\C . 2)
            '(#\a . 3)
            '(#\B . 4)
            '(#\c . 5)
            '(#\A . 6)
            '(#\b . 7)
            '(#\C . 8)
            '(#\a . 9)
            '(#\B . 10)
            '(#\c . 11))
    :key #'car
    :start 3)
   (vector '(#\A . 0)
           '(#\b . 1)
           '(#\C . 2)
           '(#\A . 6)
           '(#\b . 7)
           '(#\C . 8)
           '(#\a . 9)
           '(#\B . 10)
           '(#\c . 11)))))
(define-test sacla-must-sequence.3807 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates
    (vector '(#\A . 0)
            '(#\b . 1)
            '(#\C . 2)
            '(#\a . 3)
            '(#\B . 4)
            '(#\c . 5)
            '(#\A . 6)
            '(#\b . 7)
            '(#\C . 8)
            '(#\a . 9)
            '(#\B . 10)
            '(#\c . 11))
    :key #'car
    :start 3
    :end nil)
   (vector '(#\A . 0)
           '(#\b . 1)
           '(#\C . 2)
           '(#\A . 6)
           '(#\b . 7)
           '(#\C . 8)
           '(#\a . 9)
           '(#\B . 10)
           '(#\c . 11)))))
(define-test sacla-must-sequence.3808 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates
    (vector '(#\A . 0)
            '(#\b . 1)
            '(#\C . 2)
            '(#\a . 3)
            '(#\B . 4)
            '(#\c . 5)
            '(#\A . 6)
            '(#\b . 7)
            '(#\C . 8)
            '(#\a . 9)
            '(#\B . 10)
            '(#\c . 11))
    :key #'car
    :start 3
    :from-end t)
   (vector '(#\A . 0)
           '(#\b . 1)
           '(#\C . 2)
           '(#\a . 3)
           '(#\B . 4)
           '(#\c . 5)
           '(#\A . 6)
           '(#\b . 7)
           '(#\C . 8)))))
(define-test sacla-must-sequence.3809 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates
    (vector '(#\A . 0)
            '(#\b . 1)
            '(#\C . 2)
            '(#\a . 3)
            '(#\B . 4)
            '(#\c . 5)
            '(#\A . 6)
            '(#\b . 7)
            '(#\C . 8)
            '(#\a . 9)
            '(#\B . 10)
            '(#\c . 11))
    :key #'car
    :end 9)
   (vector '(#\a . 3)
           '(#\B . 4)
           '(#\c . 5)
           '(#\A . 6)
           '(#\b . 7)
           '(#\C . 8)
           '(#\a . 9)
           '(#\B . 10)
           '(#\c . 11)))))
(define-test sacla-must-sequence.3810 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates
    (vector '(#\A . 0)
            '(#\b . 1)
            '(#\C . 2)
            '(#\a . 3)
            '(#\B . 4)
            '(#\c . 5)
            '(#\A . 6)
            '(#\b . 7)
            '(#\C . 8)
            '(#\a . 9)
            '(#\B . 10)
            '(#\c . 11))
    :key #'car
    :end 9
    :from-end t)
   (vector '(#\A . 0)
           '(#\b . 1)
           '(#\C . 2)
           '(#\a . 3)
           '(#\B . 4)
           '(#\c . 5)
           '(#\a . 9)
           '(#\B . 10)
           '(#\c . 11)))))
(define-test sacla-must-sequence.3811 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates
    (vector '(#\A . 0)
            '(#\b . 1)
            '(#\C . 2)
            '(#\a . 3)
            '(#\B . 4)
            '(#\c . 5)
            '(#\A . 6)
            '(#\b . 7)
            '(#\C . 8)
            '(#\a . 9)
            '(#\B . 10)
            '(#\c . 11))
    :key #'car
    :start 0
    :end 12
    :test #'char-equal)
   (vector '(#\a . 9) '(#\B . 10) '(#\c . 11)))))
(define-test sacla-must-sequence.3812 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates
    (vector '(#\A . 0)
            '(#\b . 1)
            '(#\C . 2)
            '(#\a . 3)
            '(#\B . 4)
            '(#\c . 5)
            '(#\A . 6)
            '(#\b . 7)
            '(#\C . 8)
            '(#\a . 9)
            '(#\B . 10)
            '(#\c . 11))
    :key #'car
    :start 0
    :end 12
    :test-not (complement #'char-equal))
   (vector '(#\a . 9) '(#\B . 10) '(#\c . 11)))))
(define-test sacla-must-sequence.3813 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates
    (vector '(#\A . 0)
            '(#\b . 1)
            '(#\C . 2)
            '(#\a . 3)
            '(#\B . 4)
            '(#\c . 5)
            '(#\A . 6)
            '(#\b . 7)
            '(#\C . 8)
            '(#\a . 9)
            '(#\B . 10)
            '(#\c . 11))
    :key #'car
    :start 0
    :end 12
    :test #'char-equal
    :from-end t)
   #((#\A . 0) (#\b . 1) (#\C . 2)))))
(define-test sacla-must-sequence.3814 (:tag :sacla)
 (assert-true
  (equalp
   (remove-duplicates
    (vector '(#\A . 0)
            '(#\b . 1)
            '(#\C . 2)
            '(#\a . 3)
            '(#\B . 4)
            '(#\c . 5)
            '(#\A . 6)
            '(#\b . 7)
            '(#\C . 8)
            '(#\a . 9)
            '(#\B . 10)
            '(#\c . 11))
    :key #'car
    :start 0
    :end 12
    :test-not (complement #'char-equal)
    :from-end t)
   #((#\A . 0) (#\b . 1) (#\C . 2)))))
(define-test sacla-must-sequence.3815 (:tag :sacla)
 (assert-true (string= (remove-duplicates (copy-seq "")) "")))
(define-test sacla-must-sequence.3816 (:tag :sacla)
 (assert-true (string= (remove-duplicates (copy-seq "abc")) "abc")))
(define-test sacla-must-sequence.3817 (:tag :sacla)
 (assert-true (string= (remove-duplicates (copy-seq "abcabc")) "abc")))
(define-test sacla-must-sequence.3818 (:tag :sacla)
 (assert-true (string= (remove-duplicates (copy-seq "cbaabc")) "abc")))
(define-test sacla-must-sequence.3819 (:tag :sacla)
 (assert-true
  (string= (remove-duplicates (copy-seq "cbaabc") :from-end t) "cba")))
(define-test sacla-must-sequence.3820 (:tag :sacla)
 (assert-true (string= (remove-duplicates (copy-seq "cbaabcABCCBA")) "abcCBA")))
(define-test sacla-must-sequence.3821 (:tag :sacla)
 (assert-true
  (string= (remove-duplicates (copy-seq "cbaabcABCCBA") :from-end t) "cbaABC")))
(define-test sacla-must-sequence.3822 (:tag :sacla)
 (assert-true
  (string= (remove-duplicates (copy-seq "cbaabcABCCBA") :key #'char-downcase)
           "CBA")))
(define-test sacla-must-sequence.3823 (:tag :sacla)
 (assert-true
  (string=
   (remove-duplicates (copy-seq "cbaabcABCCBA")
                      :key #'char-downcase
                      :from-end t)
   "cba")))
(define-test sacla-must-sequence.3824 (:tag :sacla)
 (assert-true
  (string= (remove-duplicates (copy-seq "cbaabcABCCBA") :start 3) "cbaabcCBA")))
(define-test sacla-must-sequence.3825 (:tag :sacla)
 (assert-true
  (string= (remove-duplicates (copy-seq "cbaabcABCCBA") :start 3 :from-end t)
           "cbaabcABC")))
(define-test sacla-must-sequence.3826 (:tag :sacla)
 (assert-true
  (string= (remove-duplicates (copy-seq "cbaabcABCCBA") :start 3 :end 9)
           "cbaabcABCCBA")))
(define-test sacla-must-sequence.3827 (:tag :sacla)
 (assert-true
  (string=
   (remove-duplicates (copy-seq "cbaabcABCCBA")
                      :start 3
                      :end 9
                      :key #'char-upcase)
   "cbaABCCBA")))
(define-test sacla-must-sequence.3828 (:tag :sacla)
 (assert-true
  (string=
   (remove-duplicates (copy-seq "cbaabcABCCBA")
                      :start 3
                      :end 9
                      :key #'char-upcase
                      :from-end t)
   "cbaabcCBA")))
(define-test sacla-must-sequence.3829 (:tag :sacla)
 (assert-true
  (string=
   (remove-duplicates (copy-seq "cbaabcABCCBA")
                      :start 3
                      :end 9
                      :test #'char-equal
                      :from-end t)
   "cbaabcCBA")))
(define-test sacla-must-sequence.3830 (:tag :sacla)
 (assert-true
  (string=
   (remove-duplicates (copy-seq "cbaabcABCCBA")
                      :start 3
                      :end 9
                      :test-not (complement #'char-equal)
                      :from-end t)
   "cbaabcCBA")))
(define-test sacla-must-sequence.3831 (:tag :sacla)
 (assert-true
  (string=
   (remove-duplicates (copy-seq "cbaabcABCCBA")
                      :start 3
                      :end 9
                      :key #'upper-case-p
                      :test #'eq)
   "cbacCCBA")))
(define-test sacla-must-sequence.3832 (:tag :sacla)
 (assert-true
  (string=
   (remove-duplicates (copy-seq "cbaabcABCCBA")
                      :start 3
                      :end 9
                      :key #'upper-case-p
                      :test #'eq
                      :from-end t)
   "cbaaACBA")))
(define-test sacla-must-sequence.3833 (:tag :sacla)
 (assert-true (equal (remove-duplicates (copy-seq #*0011)) #*01)))
(define-test sacla-must-sequence.3834 (:tag :sacla)
 (assert-true (equal (remove-duplicates (copy-seq #*0110)) #*10)))
(define-test sacla-must-sequence.3835 (:tag :sacla)
 (assert-true (equal (remove-duplicates (copy-seq #*0110) :from-end t) #*01)))
(define-test sacla-must-sequence.3836 (:tag :sacla)
 (assert-true (equal (remove-duplicates (copy-seq #*0110) :start 1) #*010)))
(define-test sacla-must-sequence.3837 (:tag :sacla)
 (assert-true
  (equal (remove-duplicates (copy-seq #*0001111011000100010)) #*10)))
(define-test sacla-must-sequence.3838 (:tag :sacla)
 (assert-true
  (equal (remove-duplicates (copy-seq #*0001111011000100010) :from-end t)
         #*01)))
(define-test sacla-must-sequence.3839 (:tag :sacla)
 (assert-true (equal (remove-duplicates (copy-seq #*)) #*)))
(define-test sacla-must-sequence.3840 (:tag :sacla)
 (assert-true (equal (remove-duplicates (copy-seq #*01)) #*01)))
(define-test sacla-must-sequence.3841 (:tag :sacla)
 (assert-true (equal (remove-duplicates (copy-seq #*10)) #*10)))
(define-test sacla-must-sequence.3842 (:tag :sacla)
 (assert-true (equal (remove-duplicates (copy-seq #*0)) #*0)))
(define-test sacla-must-sequence.3843 (:tag :sacla)
 (assert-true (equal (remove-duplicates (copy-seq #*1)) #*1)))
(define-test sacla-must-sequence.3844 (:tag :sacla)
 (assert-true
  (equal (remove-duplicates (copy-seq #*1001) :start 1 :end 3) #*101)))
(define-test sacla-must-sequence.3845 (:tag :sacla)
 (assert-true
  (equal (remove-duplicates (copy-seq #*01011010) :start 2 :end 6) #*011010)))
(define-test sacla-must-sequence.3846 (:tag :sacla)
 (assert-true
  (equal (remove-duplicates (copy-seq #*01011010) :start 2 :end 6 :from-end t)
         #*010110)))
(define-test sacla-must-sequence.3847 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates (copy-seq #*01011010)
                      :start 2
                      :end 6
                      :from-end t
                      :key #'(lambda (arg) (char "aA" arg)))
   #*010110)))
(define-test sacla-must-sequence.3848 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates (copy-seq #*01011010)
                      :start 2
                      :end 6
                      :from-end t
                      :key #'(lambda (arg) (char "aA" arg))
                      :test #'char-equal)
   #*01010)))
(define-test sacla-must-sequence.3849 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates (copy-seq #*01011010)
                      :start 2
                      :end 6
                      :from-end t
                      :key #'(lambda (arg) (char "aA" arg))
                      :test-not (complement #'char-equal))
   #*01010)))
(define-test sacla-must-sequence.3850 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates (copy-seq #*01011010)
                      :start 2
                      :end 6
                      :key #'(lambda (arg) (char "aA" arg))
                      :test #'char-equal)
   #*01010)))
(define-test sacla-must-sequence.3851 (:tag :sacla)
 (assert-true
  (equal
   (remove-duplicates (copy-seq #*01011010)
                      :start 2
                      :end 6
                      :key #'(lambda (arg) (char "aA" arg))
                      :test-not (complement #'char-equal))
   #*01010)))

