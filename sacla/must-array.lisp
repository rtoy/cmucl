(in-package #:sacla-lisp-unit)
(define-test sacla-must-array.1 (:tag :sacla)
 (assert-true (arrayp (make-array nil))))
(define-test sacla-must-array.2 (:tag :sacla)
 (assert-true (arrayp (make-array 10))))
(define-test sacla-must-array.3 (:tag :sacla)
 (assert-true (vectorp (make-array 10))))
(define-test sacla-must-array.4 (:tag :sacla)
 (assert-true (arrayp (make-array '(1 2)))))
(define-test sacla-must-array.5 (:tag :sacla)
 (assert-true (arrayp (make-array '(1 2 3)))))
(define-test sacla-must-array.6 (:tag :sacla)
 (assert-true (arrayp (make-array '(1 2 3 4)))))
(define-test sacla-must-array.7 (:tag :sacla)
 (assert-true (arrayp (make-array '(1 2 3 4 5)))))
(define-test sacla-must-array.8 (:tag :sacla)
 (assert-true (arrayp (make-array '(3 3 3)))))
(define-test sacla-must-array.9 (:tag :sacla)
 (assert-true (arrayp (make-array '(3 0 3)))))
(define-test sacla-must-array.10 (:tag :sacla)
 (assert-true
  (arrayp (make-array '5 :element-type 'character :displaced-to "array"))))
(define-test sacla-must-array.11 (:tag :sacla) (assert-true (arrayp "")))
(define-test sacla-must-array.12 (:tag :sacla) (assert-true (arrayp "array")))
(define-test sacla-must-array.13 (:tag :sacla)
 (assert-true (arrayp (make-array '(2 3 4) :adjustable t))))
(define-test sacla-must-array.14 (:tag :sacla)
 (assert-true (arrayp (make-array 6))))
(define-test sacla-must-array.15 (:tag :sacla) (assert-true (arrayp #*1011)))
(define-test sacla-must-array.16 (:tag :sacla) (assert-true (arrayp "hi")))
(define-test sacla-must-array.17 (:tag :sacla) (assert-true (not (arrayp 'hi))))
(define-test sacla-must-array.18 (:tag :sacla) (assert-true (not (arrayp 12))))
(define-test sacla-must-array.19 (:tag :sacla)
 (assert-true
  (let ((array (make-array '(2 3) :initial-contents '((0 1 2) (3 4 5)))))
    (and (eql (aref array 0 0) 0)
         (eql (aref array 0 1) 1)
         (eql (aref array 0 2) 2)
         (eql (aref array 1 0) 3)
         (eql (aref array 1 1) 4)
         (eql (aref array 1 2) 5)))))
(define-test sacla-must-array.20 (:tag :sacla)
 (assert-true
  (let ((array
         (make-array '(3 2 1)
                     :initial-contents '(((0) (1)) ((2) (3)) ((4) (5))))))
    (and (eql (aref array 0 0 0) 0)
         (eql (aref array 0 1 0) 1)
         (eql (aref array 1 0 0) 2)
         (eql (aref array 1 1 0) 3)
         (eql (aref array 2 0 0) 4)
         (eql (aref array 2 1 0) 5)))))
(define-test sacla-must-array.21 (:tag :sacla)
 (assert-true
  (let ((array
         (make-array '(2 2 2 2)
                     :initial-contents '((((0 1) (2 3)) ((4 5) (6 7)))
                                         (((8 9) (10 11)) ((12 13) (14 15)))))))
    (and (eql (aref array 0 0 0 0) 0)
         (eql (aref array 0 0 0 1) 1)
         (eql (aref array 0 0 1 0) 2)
         (eql (aref array 0 0 1 1) 3)
         (eql (aref array 0 1 0 0) 4)
         (eql (aref array 0 1 0 1) 5)
         (eql (aref array 0 1 1 0) 6)
         (eql (aref array 0 1 1 1) 7)
         (eql (aref array 1 0 0 0) 8)
         (eql (aref array 1 0 0 1) 9)
         (eql (aref array 1 0 1 0) 10)
         (eql (aref array 1 0 1 1) 11)
         (eql (aref array 1 1 0 0) 12)
         (eql (aref array 1 1 0 1) 13)
         (eql (aref array 1 1 1 0) 14)
         (eql (aref array 1 1 1 1) 15)))))
(define-test sacla-must-array.22 (:tag :sacla)
 (assert-true
  (let ((array (make-array '(3 3 3 3 3 3) :initial-element nil)))
    (dotimes (i 729) (setf (row-major-aref array i) i))
    (dotimes (i 729 t)
      (unless
          (=
           (aref array
                 (floor i (* 3 3 3 3 3))
                 (floor (mod i (* 3 3 3 3 3)) (* 3 3 3 3))
                 (floor (mod i (* 3 3 3 3)) (* 3 3 3))
                 (floor (mod i (* 3 3 3)) (* 3 3))
                 (floor (mod i (* 3 3)) (* 3))
                 (mod i 3))
           i)
        (return nil))))))
(define-test sacla-must-array.23 (:tag :sacla)
 (assert-true (zerop (aref (make-array 'nil :initial-contents 0)))))
(define-test sacla-must-array.24 (:tag :sacla)
 (assert-true
  (let ((array (make-array 10 :initial-contents '(0 1 2 3 4 5 6 7 8 9))) (ok t))
    (dotimes (i 10)
      (unless (eql (aref array i) i)
        (setq ok nil)
        (return)))
    ok)))
(define-test sacla-must-array.25 (:tag :sacla)
 (assert-true
  (let ((array (vector 0 1 2 3 4 5 6 7 8 9)) (ok t))
    (dotimes (i 10)
      (unless (eql (aref array i) i)
        (setq ok nil)
        (return)))
    ok)))
(define-test sacla-must-array.26 (:tag :sacla)
 (assert-true
  (let ((array "0123456789") (ok t))
    (dotimes (i 10)
      (unless (char= (aref array i) (char "0123456789" i))
        (setq ok nil)
        (return)))
    ok)))
(define-test sacla-must-array.27 (:tag :sacla)
 (assert-true
  (let ((array (make-array '(2 3) :initial-contents '((0 1 2) (3 4 5)))))
    (equal (array-dimensions array) '(2 3)))))
(define-test sacla-must-array.28 (:tag :sacla)
 (assert-true (equal (array-dimensions (make-array 4)) '(4))))
(define-test sacla-must-array.29 (:tag :sacla)
 (assert-true (equal (array-dimensions (make-array '(2 3))) '(2 3))))
(define-test sacla-must-array.30 (:tag :sacla)
 (assert-true (equal (array-dimensions (make-array 4 :fill-pointer 2)) '(4))))
(define-test sacla-must-array.31 (:tag :sacla)
 (assert-true
  (equal (array-dimensions (make-array '(2 3 4 5 6))) '(2 3 4 5 6))))
(define-test sacla-must-array.32 (:tag :sacla)
 (assert-true (eql (array-dimension (make-array 4) 0) 4)))
(define-test sacla-must-array.33 (:tag :sacla)
 (assert-true (eql (array-dimension (make-array '(2 3)) 1) 3)))
(define-test sacla-must-array.34 (:tag :sacla)
 (assert-true (eql (array-dimension (make-array '(2 3 4)) 2) 4)))
(define-test sacla-must-array.35 (:tag :sacla)
 (assert-true (eq (array-element-type (make-array 4)) t)))
(define-test sacla-must-array.36 (:tag :sacla)
 (assert-true
  (equal (array-element-type (make-array 12 :element-type '(unsigned-byte 8)))
         (upgraded-array-element-type '(unsigned-byte 8)))))
(define-test sacla-must-array.37 (:tag :sacla)
 (assert-true
  (let ((array (make-array 'nil)))
    (multiple-value-bind (displaced-to displaced-index-offset)
        (array-displacement array)
      (and (not displaced-to) (zerop displaced-index-offset))))))
(define-test sacla-must-array.38 (:tag :sacla)
 (assert-true
  (let ((array (make-array '10)))
    (multiple-value-bind (displaced-to displaced-index-offset)
        (array-displacement array)
      (and (not displaced-to) (zerop displaced-index-offset))))))
(define-test sacla-must-array.39 (:tag :sacla)
 (assert-true
  (let ((array (make-array '(2 3) :initial-contents '((0 1 2) (3 4 5)))))
    (multiple-value-bind (displaced-to displaced-index-offset)
        (array-displacement array)
      (and (not displaced-to) (zerop displaced-index-offset))))))
(define-test sacla-must-array.40 (:tag :sacla)
 (assert-true
  (let* ((source
          (make-array '(2 5)
                      :initial-contents '((1 2 3 4 5) (11 12 13 14 15))))
         (array (make-array 10 :displaced-to source)))
    (multiple-value-bind (displaced-to displaced-index-offset)
        (array-displacement array)
      (and (eq displaced-to source) (zerop displaced-index-offset))))))
(define-test sacla-must-array.41 (:tag :sacla)
 (assert-true
  (let* ((source (make-array '10 :initial-element 0))
         (array (make-array '(5 2) :displaced-to source)))
    (multiple-value-bind (displaced-to displaced-index-offset)
        (array-displacement array)
      (and (eq displaced-to source) (zerop displaced-index-offset))))))
(define-test sacla-must-array.42 (:tag :sacla)
 (assert-true
  (let* ((e0-0 (list 0 0))
         (e0-1 (list 0 1))
         (e1-0 (list 1 0))
         (e1-1 (list 1 1))
         (source
          (make-array '(2 2)
                      :initial-contents (list (list e0-0 e0-1)
                                              (list e1-0 e1-1))))
         (array (make-array 4 :displaced-to source)))
    (multiple-value-bind (displaced-to displaced-index-offset)
        (array-displacement array)
      (and (eq displaced-to source)
           (zerop displaced-index-offset)
           (eq (aref array 0) e0-0)
           (eq (aref array 1) e0-1)
           (eq (aref array 2) e1-0)
           (eq (aref array 3) e1-1))))))
(define-test sacla-must-array.43 (:tag :sacla)
 (assert-true
  (let* ((e0-0 (list 0 0))
         (e0-1 (list 0 1))
         (e1-0 (list 1 0))
         (e1-1 (list 1 1))
         (source
          (make-array '(2 2)
                      :initial-contents (list (list e0-0 e0-1)
                                              (list e1-0 e1-1))))
         (array (make-array 2 :displaced-to source :displaced-index-offset 1)))
    (multiple-value-bind (displaced-to displaced-index-offset)
        (array-displacement array)
      (and (eq displaced-to source)
           (eql displaced-index-offset 1)
           (eq (aref array 0) e0-1)
           (eq (aref array 1) e1-0))))))
(define-test sacla-must-array.44 (:tag :sacla)
 (assert-true
  (let ((array
         (make-array 4
                     :element-type 'character
                     :displaced-to "0123456789"
                     :displaced-index-offset 6)))
    (multiple-value-bind (displaced-to displaced-index-offset)
        (array-displacement array)
      (and (string= displaced-to "0123456789")
           (eql displaced-index-offset 6)
           (eql (aref array 0) #\6)
           (eql (aref array 1) #\7)
           (eql (aref array 2) #\8)
           (eql (aref array 3) #\9))))))
(define-test sacla-must-array.45 (:tag :sacla)
 (assert-true
  (let ((array
         (make-array '(1 2 5)
                     :element-type 'character
                     :displaced-to "0123456789")))
    (multiple-value-bind (displaced-to displaced-index-offset)
        (array-displacement array)
      (and (string= displaced-to "0123456789")
           (eql displaced-index-offset 0)
           (eql (aref array 0 0 0) #\0)
           (eql (aref array 0 0 1) #\1)
           (eql (aref array 0 0 2) #\2)
           (eql (aref array 0 0 3) #\3)
           (eql (aref array 0 0 4) #\4)
           (eql (aref array 0 1 0) #\5)
           (eql (aref array 0 1 1) #\6)
           (eql (aref array 0 1 2) #\7)
           (eql (aref array 0 1 3) #\8)
           (eql (aref array 0 1 4) #\9))))))
(define-test sacla-must-array.46 (:tag :sacla)
 (assert-true
  (let* ((source
          (make-array '(2 5)
                      :initial-contents '("love&" "peace")
                      :element-type 'character))
         (array (make-array 10 :displaced-to source :element-type 'character)))
    (multiple-value-bind (displaced-to displaced-index-offset)
        (array-displacement array)
      (and (eq displaced-to source)
           (eql displaced-index-offset 0)
           (string= array "love&peace"))))))
(define-test sacla-must-array.47 (:tag :sacla)
 (assert-true (array-in-bounds-p (make-array 5) 4)))
(define-test sacla-must-array.48 (:tag :sacla)
 (assert-true (not (array-in-bounds-p (make-array 5) -1))))
(define-test sacla-must-array.49 (:tag :sacla)
 (assert-true
  (let ((a (make-array '(7 11) :element-type 'string-char)))
    (and (array-in-bounds-p a 0 0)
         (array-in-bounds-p a 6 10)
         (not (array-in-bounds-p a 0 -1))
         (not (array-in-bounds-p a 0 11))
         (not (array-in-bounds-p a 7 0))))))
(define-test sacla-must-array.50 (:tag :sacla)
 (assert-true
  (let ((array (make-array '(2 3) :initial-contents '((0 1 2) (3 4 5)))))
    (eql (array-rank array) 2))))
(define-test sacla-must-array.51 (:tag :sacla)
 (assert-true (zerop (array-rank (make-array 'nil)))))
(define-test sacla-must-array.52 (:tag :sacla)
 (assert-true (eql (array-rank (make-array 10)) 1)))
(define-test sacla-must-array.53 (:tag :sacla)
 (assert-true (eql (array-rank (make-array '(2 10))) 2)))
(define-test sacla-must-array.54 (:tag :sacla)
 (assert-true (eql (array-rank (make-array '(2 10 1))) 3)))
(define-test sacla-must-array.55 (:tag :sacla)
 (assert-true (eql (array-rank (make-array '(2 10 1 3))) 4)))
(define-test sacla-must-array.56 (:tag :sacla)
 (assert-true (eql (array-rank "") 1)))
(define-test sacla-must-array.57 (:tag :sacla)
 (assert-true (eql (array-rank "a") 1)))
(define-test sacla-must-array.58 (:tag :sacla)
 (assert-true (zerop (array-row-major-index (make-array 'nil)))))
(define-test sacla-must-array.59 (:tag :sacla)
 (assert-true (zerop (array-row-major-index (make-array '5) 0))))
(define-test sacla-must-array.60 (:tag :sacla)
 (assert-true (eql (array-row-major-index (make-array '5) 4) 4)))
(define-test sacla-must-array.61 (:tag :sacla)
 (assert-true (eql (array-row-major-index (make-array '10) 3) 3)))
(define-test sacla-must-array.62 (:tag :sacla)
 (assert-true (zerop (array-row-major-index (make-array '(3 4)) 0 0))))
(define-test sacla-must-array.63 (:tag :sacla)
 (assert-true (eql (array-row-major-index (make-array '(3 4)) 2 3) 11)))
(define-test sacla-must-array.64 (:tag :sacla)
 (assert-true (zerop (array-row-major-index (make-array '(3 4 5)) 0 0 0))))
(define-test sacla-must-array.65 (:tag :sacla)
 (assert-true (eql (array-row-major-index (make-array '(3 4 5)) 2 3 4) 59)))
(define-test sacla-must-array.66 (:tag :sacla)
 (assert-true
  (let ((array (make-array '(2 3) :initial-contents '((0 1 2) (3 4 5)))))
    (eql (array-total-size array) 6))))
(define-test sacla-must-array.67 (:tag :sacla)
 (assert-true (eql (array-total-size (make-array 4)) 4)))
(define-test sacla-must-array.68 (:tag :sacla)
 (assert-true (eql (array-total-size (make-array 4 :fill-pointer 2)) 4)))
(define-test sacla-must-array.69 (:tag :sacla)
 (assert-true (eql (array-total-size (make-array 0)) 0)))
(define-test sacla-must-array.70 (:tag :sacla)
 (assert-true (eql (array-total-size (make-array '(4 2))) 8)))
(define-test sacla-must-array.71 (:tag :sacla)
 (assert-true (eql (array-total-size (make-array '(4 0))) 0)))
(define-test sacla-must-array.72 (:tag :sacla)
 (assert-true (eql (array-total-size (make-array 'nil)) 1)))
(define-test sacla-must-array.73 (:tag :sacla)
 (assert-true (eql (array-total-size (make-array '(2 3 4 5))) (* 2 3 4 5))))
(define-test sacla-must-array.74 (:tag :sacla)
 (assert-true
  (let ((array
         (make-array 10
                     :initial-contents '(0 1 2 3 4 5 6 7 8 9)
                     :fill-pointer 0)))
    (dotimes (i 10 t)
      (unless (eql (aref array i) i)
        (return nil))))))
(define-test sacla-must-array.75 (:tag :sacla)
 (assert-true
  (let ((array (make-array '(10 10) :element-type 'number :initial-element 0)))
    (dotimes (i 10)
      (dotimes (j 10)
        (unless (zerop (aref array i j))
          (return nil))
        (setf (aref array i j) (+ (* i 10) j))))
    (dotimes (i 100 t)
      (unless (eql (row-major-aref array i) i)
        (return nil))))))
(define-test sacla-must-array.76 (:tag :sacla)
 (assert-true
  (let ((array (make-array 'nil)))
    (setf (aref array) 100)
    (eql (aref array) 100))))
(define-test sacla-must-array.77 (:tag :sacla)
 (assert-true
  (let ((array (make-array 10 :initial-contents '(a b c d e f g h i j))))
    (setf (aref array 0) #\a)
    (setf (aref array 2) #\c)
    (setf (aref array 4) #\e)
    (setf (aref array 6) #\g)
    (setf (aref array 8) #\i)
    (and (eql (aref array 0) #\a)
         (eql (aref array 1) 'b)
         (eql (aref array 2) #\c)
         (eql (aref array 3) 'd)
         (eql (aref array 4) #\e)
         (eql (aref array 5) 'f)
         (eql (aref array 6) #\g)
         (eql (aref array 7) 'h)
         (eql (aref array 8) #\i)
         (eql (aref array 9) 'j)))))
(define-test sacla-must-array.78 (:tag :sacla)
 (assert-true
  (let ((array (vector 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)))
    (setf (aref array 0) #\a)
    (setf (aref array 2) #\c)
    (setf (aref array 4) #\e)
    (setf (aref array 6) #\g)
    (setf (aref array 8) #\i)
    (and (eql (aref array 0) #\a)
         (eql (aref array 1) 'b)
         (eql (aref array 2) #\c)
         (eql (aref array 3) 'd)
         (eql (aref array 4) #\e)
         (eql (aref array 5) 'f)
         (eql (aref array 6) #\g)
         (eql (aref array 7) 'h)
         (eql (aref array 8) #\i)
         (eql (aref array 9) 'j)))))
(define-test sacla-must-array.79 (:tag :sacla)
 (assert-true
  (let ((array (make-array '(3 4 5) :initial-element 0 :element-type 'number)))
    (setf (aref array 0 0 0) 0)
    (setf (aref array 1 1 1) 1)
    (setf (aref array 2 2 2) 2)
    (dotimes (i 3 t)
      (unless (eql (aref array i i i) i)
        (return nil))))))
(define-test sacla-must-array.80 (:tag :sacla)
 (assert-true
  (let* ((array
          (make-array '(3 4 5 6 7) :initial-element 0 :element-type 'number))
         (array2 (make-array (* 3 4 5 6 7) :displaced-to array)))
    (setf (aref array 2 3 4 5 6) 100)
    (setf (aref array 0 0 0 0 0) 200)
    (eql (reduce #'+ array2) 300))))
(define-test sacla-must-array.81 (:tag :sacla)
 (assert-true
  (adjustable-array-p
   (make-array 5 :element-type 'character :adjustable t :fill-pointer 3))))
(define-test sacla-must-array.82 (:tag :sacla)
 (assert-true
  (let ((array
         (adjust-array
          (make-array '(2 3)
                      :initial-contents '((0 1 2) (3 4 5))
                      :adjustable t)
          '(3 2)
          :initial-element 'undefined)))
    (and (eql (aref array 0 0) 0)
         (eql (aref array 0 1) 1)
         (eql (aref array 1 0) 3)
         (eql (aref array 1 1) 4)
         (eql (aref array 2 0) 'undefined)
         (eql (aref array 2 1) 'undefined)))))
(define-test sacla-must-array.83 (:tag :sacla)
 (assert-true
  (let ((array
         (adjust-array
          (make-array '(2 3)
                      :initial-contents '((0 1 2) (3 4 5))
                      :adjustable t)
          '(3 2)
          :initial-element 'undefined)))
    (equal (array-dimensions array) '(3 2)))))
(define-test sacla-must-array.84 (:tag :sacla)
 (assert-true
  (let ((array
         (adjust-array
          (make-array '(2 3)
                      :initial-contents '((0 1 2) (3 4 5))
                      :adjustable t)
          '(3 2)
          :initial-element 'undefined)))
    (not (array-has-fill-pointer-p array)))))
(define-test sacla-must-array.85 (:tag :sacla)
 (assert-true
  (let ((array (make-array '(2 3) :initial-contents '((0 1 2) (3 4 5)))))
    (not (array-has-fill-pointer-p array)))))
(define-test sacla-must-array.86 (:tag :sacla)
 (assert-true (array-has-fill-pointer-p (make-array 10 :fill-pointer 0))))
(define-test sacla-must-array.87 (:tag :sacla)
 (assert-true
  (array-has-fill-pointer-p (make-array 8 :fill-pointer 0 :initial-element 8))))
(define-test sacla-must-array.88 (:tag :sacla)
 (assert-true (not (array-has-fill-pointer-p (make-array '(2 3 4))))))
(define-test sacla-must-array.89 (:tag :sacla)
 (assert-true
  (let ((array
         (adjust-array
          (make-array '(2 3)
                      :initial-contents '((0 1 2) (3 4 5))
                      :adjustable t)
          '(3 2)
          :initial-element 'undefined)))
    (multiple-value-bind (displaced-to displaced-index-offset)
        (array-displacement array)
      (and (not displaced-to) (zerop displaced-index-offset))))))
(define-test sacla-must-array.90 (:tag :sacla)
 (assert-true
  (let ((array
         (adjust-array
          (make-array '(2 3)
                      :initial-contents '((0 1 2) (3 4 5))
                      :adjustable t)
          '(3 2)
          :initial-element 'undefined)))
    (eql (array-rank array) 2))))
(define-test sacla-must-array.91 (:tag :sacla)
 (assert-true
  (let ((array
         (adjust-array
          (make-array '(2 3)
                      :initial-contents '((0 1 2) (3 4 5))
                      :adjustable t)
          '(3 2)
          :initial-element 'undefined)))
    (eql (array-total-size array) 6))))
(define-test sacla-must-array.92 (:tag :sacla)
 (assert-true (eql (fill-pointer (make-array 8 :fill-pointer 4)) 4)))
(define-test sacla-must-array.93 (:tag :sacla)
 (assert-true
  (let ((array (make-array 8 :fill-pointer 4 :initial-element nil)))
    (and (eql (length array) 4)
         (setf (fill-pointer array) 3)
         (eql (fill-pointer array) 3)
         (eql (length array) 3)))))
(define-test sacla-must-array.94 (:tag :sacla)
 (assert-true
  (let ((vector
         (make-array 10
                     :fill-pointer 0
                     :initial-element #\
                     :element-type 'character)))
    (and (eql (vector-push #\a vector) 0)
         (eql (vector-push #\b vector) 1)
         (eql (vector-push #\c vector) 2)
         (string= vector "abc")))))
(define-test sacla-must-array.95 (:tag :sacla)
 (assert-true
  (let ((vector (make-array 3 :fill-pointer t :initial-contents '(a b c))))
    (and (eql (array-dimension vector 0) (fill-pointer vector))
         (equal (concatenate 'list vector) '(a b c))
         (zerop (setf (fill-pointer vector) 0))
         (null (concatenate 'list vector))
         (eql (vector-push 'x vector) 0)
         (equal (concatenate 'list vector) '(x))
         (eq (vector-pop vector) 'x)
         (zerop (length vector))))))
(define-test sacla-must-array.96 (:tag :sacla)
 (assert-true
  (let ((vector (make-array 10 :fill-pointer 0 :initial-element nil)))
    (and (eql (length vector) 0)
         (setf (fill-pointer vector) 10)
         (eql (length vector) 10)
         (setf (fill-pointer vector) 5)
         (eql (length vector) 5)))))
(define-test sacla-must-array.97 (:tag :sacla)
 (assert-true
  (let ((array
         (make-array '(3 2 1)
                     :initial-contents '(((0) (1)) ((2) (3)) ((4) (5))))))
    (and (eql (aref array 0 0 0) (row-major-aref array 0))
         (eql (aref array 0 1 0) (row-major-aref array 1))
         (eql (aref array 1 0 0) (row-major-aref array 2))
         (eql (aref array 1 1 0) (row-major-aref array 3))
         (eql (aref array 2 0 0) (row-major-aref array 4))
         (eql (aref array 2 1 0) (row-major-aref array 5))))))
(define-test sacla-must-array.98 (:tag :sacla)
 (assert-true
  (let ((array
         (make-array '(3 2 1)
                     :initial-contents '(((0) (1)) ((2) (3)) ((4) (5))))))
    (and (eql 0 (row-major-aref array 0))
         (eql 1 (row-major-aref array 1))
         (eql 2 (row-major-aref array 2))
         (eql 3 (row-major-aref array 3))
         (eql 4 (row-major-aref array 4))
         (eql 5 (row-major-aref array 5))))))
(define-test sacla-must-array.99 (:tag :sacla)
 (assert-true
  (let* ((array0
          (make-array '(3 2 1)
                      :initial-contents '(((0) (1)) ((2) (3)) ((4) (5)))))
         (array1 (make-array 6 :displaced-to array0)))
    (and (eql (aref array1 0) (row-major-aref array0 0))
         (eql (aref array1 1) (row-major-aref array0 1))
         (eql (aref array1 2) (row-major-aref array0 2))
         (eql (aref array1 3) (row-major-aref array0 3))
         (eql (aref array1 4) (row-major-aref array0 4))
         (eql (aref array1 5) (row-major-aref array0 5))))))
(define-test sacla-must-array.100 (:tag :sacla)
 (assert-true
  (let* ((array0
          (make-array 6 :element-type 'character :initial-contents "abcdef"))
         (array1
          (make-array '(3 2 1) :displaced-to array0 :element-type 'character)))
    (and (eql (aref array0 0) (row-major-aref array1 0))
         (eql (aref array0 1) (row-major-aref array1 1))
         (eql (aref array0 2) (row-major-aref array1 2))
         (eql (aref array0 3) (row-major-aref array1 3))
         (eql (aref array0 4) (row-major-aref array1 4))
         (eql (aref array0 5) (row-major-aref array1 5))))))
(define-test sacla-must-array.101 (:tag :sacla)
 (assert-true
  (let* ((array0
          (make-array 6 :element-type 'character :initial-contents "abcdef"))
         (array1
          (make-array '(3 2 1) :displaced-to array0 :element-type 'character)))
    (and (eql #\a (row-major-aref array1 0))
         (eql #\b (row-major-aref array1 1))
         (eql #\c (row-major-aref array1 2))
         (eql #\d (row-major-aref array1 3))
         (eql #\e (row-major-aref array1 4))
         (eql #\f (row-major-aref array1 5))))))
(define-test sacla-must-array.102 (:tag :sacla)
 (assert-true
  (let ((array (make-array '(3 2 1) :initial-element nil)))
    (setf (row-major-aref array 0) 'a)
    (setf (row-major-aref array 1) 'b)
    (setf (row-major-aref array 2) 'c)
    (setf (row-major-aref array 3) 'd)
    (setf (row-major-aref array 4) 'e)
    (and (eql (aref array 0 0 0) 'a)
         (eql (aref array 0 1 0) 'b)
         (eql (aref array 1 0 0) 'c)
         (eql (aref array 1 1 0) 'd)
         (eql (aref array 2 0 0) 'e)
         (eql (aref array 2 1 0) 'nil)))))
(define-test sacla-must-array.103 (:tag :sacla)
 (assert-true
  (let ((str "abcdefg"))
    (dotimes (i 7 t)
      (unless (eql (char str 0) (row-major-aref str 0))
        (return nil))))))
(define-test sacla-must-array.104 (:tag :sacla)
 (assert-true
  (let ((str (make-array 5 :initial-contents "abcde")))
    (dotimes (i 3) (setf (row-major-aref str i) (row-major-aref str (- 4 i))))
    (and (char= (row-major-aref str 0) #\e)
         (char= (row-major-aref str 1) #\d)
         (char= (row-major-aref str 2) #\c)
         (char= (row-major-aref str 3) #\d)
         (char= (row-major-aref str 4) #\e)))))
(define-test sacla-must-array.105 (:tag :sacla)
 (assert-true (eq (upgraded-array-element-type t) t)))
(define-test sacla-must-array.106 (:tag :sacla)
 (assert-true
  (and (subtypep (upgraded-array-element-type 'bit) 'bit)
       (subtypep 'bit (upgraded-array-element-type 'bit)))))
(define-test sacla-must-array.107 (:tag :sacla)
 (assert-true
  (and (subtypep (upgraded-array-element-type 'base-char) 'base-char)
       (subtypep 'base-char (upgraded-array-element-type 'base-char)))))
(define-test sacla-must-array.108 (:tag :sacla)
 (assert-true
  (and (subtypep (upgraded-array-element-type 'character) 'character)
       (subtypep 'character (upgraded-array-element-type 'character)))))
(define-test sacla-must-array.109 (:tag :sacla)
 (assert-true (simple-vector-p (make-array 6))))
(define-test sacla-must-array.110 (:tag :sacla)
 (assert-true (not (simple-vector-p "aaaaaa"))))
(define-test sacla-must-array.111 (:tag :sacla)
 (assert-true
  (let ((sv (make-array 10)))
    (dotimes (i 10) (setf (svref sv i) (* i i)))
    (dotimes (i 10 t)
      (unless (eql (svref sv i) (* i i))
        (return nil))))))
(define-test sacla-must-array.112 (:tag :sacla)
 (assert-true
  (let ((sv (vector 'a 'b 'c 'd 'e 'f)))
    (and (eq (svref sv 0) 'a)
         (eq (svref sv 1) 'b)
         (eq (svref sv 2) 'c)
         (eq (svref sv 3) 'd)
         (eq (svref sv 4) 'e)
         (eq (svref sv 5) 'f)))))
(define-test sacla-must-array.113 (:tag :sacla)
 (assert-true
  (let ((sv (make-array 3 :initial-contents '(1 2 last))))
    (and (simple-vector-p sv)
         (eq (svref sv 2) 'last)
         (eql (svref sv 1) 2)
         (eql (svref sv 0) 1)
         (eql (setf (svref sv 1) 'last-but-one) 'last-but-one)
         (eq (svref sv 1) 'last-but-one)))))
(define-test sacla-must-array.114 (:tag :sacla)
 (assert-true
  (let ((vec (vector 1 2 'last)))
    (and (arrayp vec)
         (vectorp vec)
         (simple-vector-p vec)
         (eql (length vec) 3)
         (equal (concatenate 'list vec) '(1 2 last))))))
(define-test sacla-must-array.115 (:tag :sacla)
 (assert-true
  (eq (vector-pop (make-array 3 :initial-contents '(a b c) :fill-pointer t))
      'c)))
(define-test sacla-must-array.116 (:tag :sacla)
 (assert-true
  (eq (vector-pop (make-array 3 :initial-contents '(a b c) :fill-pointer 3))
      'c)))
(define-test sacla-must-array.117 (:tag :sacla)
 (assert-true
  (eq (vector-pop (make-array 3 :initial-contents '(a b c) :fill-pointer 2))
      'b)))
(define-test sacla-must-array.118 (:tag :sacla)
 (assert-true
  (eq (vector-pop (make-array 3 :initial-contents '(a b c) :fill-pointer 1))
      'a)))
(define-test sacla-must-array.119 (:tag :sacla)
 (assert-true
  (let ((vec (make-array 3 :fill-pointer 0)))
    (and (eql (vector-push 'a vec) 0)
         (eql (vector-push 'b vec) 1)
         (eql (vector-push 'c vec) 2)
         (eq (vector-pop vec) 'c)
         (eq (vector-pop vec) 'b)
         (eq (vector-pop vec) 'a)))))
(define-test sacla-must-array.120 (:tag :sacla)
 (assert-true
  (let ((vec (make-array 3 :fill-pointer t :initial-contents '(a b c))))
    (and (setf (fill-pointer vec) 1)
         (eql (vector-push 'y vec) 1)
         (eql (vector-push 'z vec) 2)
         (eq (vector-pop vec) 'z)
         (eq (vector-pop vec) 'y)
         (eq (vector-pop vec) 'a)
         (eql (fill-pointer vec) 0)))))
(define-test sacla-must-array.121 (:tag :sacla)
 (assert-true
  (let ((vec (make-array 3 :fill-pointer t :initial-contents '(a b c))))
    (and (not (vector-push 'x vec))
         (not (vector-push 'y vec))
         (eql (setf (fill-pointer vec) 0) 0)
         (eql (vector-push 'x vec) 0)
         (eql (vector-push 'y vec) 1)
         (eql (vector-push 'z vec) 2)
         (not (vector-push 'l vec))))))
(define-test sacla-must-array.122 (:tag :sacla)
 (assert-true
  (let ((vec
         (make-array 3
                     :fill-pointer 2
                     :initial-contents '(a b l)
                     :adjustable t)))
    (and (eql (length vec) 2)
         (eql (vector-push-extend 'c vec) 2)
         (eql (length vec) 3)
         (eq (vector-pop vec) 'c)
         (eql (vector-push-extend 'c vec) 2)
         (eql (vector-push-extend 'x vec) 3)
         (eql (vector-push-extend 'y vec) 4)
         (eql (vector-push-extend 'z vec) 5)
         (eql (length vec) 6)))))
(define-test sacla-must-array.123 (:tag :sacla)
 (assert-true
  (let ((vec (make-array 0 :fill-pointer t :adjustable t)))
    (dotimes (i 50) (vector-push-extend (* i i) vec))
    (dotimes (i 50 t)
      (unless (eql (vector-pop vec) (* (- 49 i) (- 49 i)))
        (return nil))))))
(define-test sacla-must-array.124 (:tag :sacla)
 (assert-true
  (let ((vec
         (make-array 10
                     :element-type 'character
                     :initial-contents "abcdefghij"
                     :adjustable t
                     :fill-pointer t)))
    (and (eql (vector-push-extend #\x vec) 10)
         (eql (vector-push-extend #\y vec) 11)
         (eql (vector-push-extend #\z vec) 12)
         (string= vec "abcdefghijxyz")))))
(define-test sacla-must-array.125 (:tag :sacla)
 (assert-true (vectorp "aaaaaa")))
(define-test sacla-must-array.126 (:tag :sacla)
 (assert-true (vectorp (make-array 6 :fill-pointer t))))
(define-test sacla-must-array.127 (:tag :sacla)
 (assert-true (not (vectorp (make-array '(2 3 4))))))
(define-test sacla-must-array.128 (:tag :sacla) (assert-true (vectorp #*11)))
(define-test sacla-must-array.129 (:tag :sacla) (assert-true (not (vectorp 3))))
(define-test sacla-must-array.130 (:tag :sacla)
 (assert-true
  (vectorp (make-array 3 :displaced-to "abc" :element-type 'character))))
(define-test sacla-must-array.131 (:tag :sacla)
 (assert-true
  (eql (bit (make-array 8 :element-type 'bit :initial-element 1) 3) 1)))
(define-test sacla-must-array.132 (:tag :sacla)
 (assert-true
  (eql (sbit (make-array 8 :element-type 'bit :initial-element 1) 3) 1)))
(define-test sacla-must-array.133 (:tag :sacla)
 (assert-true
  (let ((ba
         (make-array 8
                     :element-type 'bit
                     :initial-contents '(0 1 0 1 0 1 0 1))))
    (dotimes (i 8 t)
      (unless
          (or (and (evenp i) (zerop (bit ba i)))
              (and (oddp i) (eql (bit ba i) 1)))
        (return nil))))))
(define-test sacla-must-array.134 (:tag :sacla)
 (assert-true
  (let ((ba
         (make-array 8
                     :element-type 'bit
                     :initial-contents '(0 1 0 1 0 1 0 1))))
    (dotimes (i 8 t)
      (unless
          (or (and (evenp i) (zerop (sbit ba i)))
              (and (oddp i) (eql (sbit ba i) 1)))
        (return nil))))))
(define-test sacla-must-array.135 (:tag :sacla)
 (assert-true
  (let ((ba
         (make-array '(3 3)
                     :element-type 'bit
                     :initial-contents '((0 1 0) (1 0 1) (0 1 0)))))
    (and (zerop (bit ba 0 0))
         (eql (bit ba 0 1) 1)
         (zerop (bit ba 0 2))
         (eql (bit ba 1 0) 1)
         (zerop (bit ba 1 1))
         (eql (bit ba 1 2) 1)
         (zerop (bit ba 2 0))
         (eql (bit ba 2 1) 1)
         (zerop (bit ba 2 2))))))
(define-test sacla-must-array.136 (:tag :sacla)
 (assert-true
  (let ((ba
         (make-array '(3 3)
                     :element-type 'bit
                     :initial-contents '((0 1 0) (1 0 1) (0 1 0)))))
    (and (zerop (sbit ba 0 0))
         (eql (sbit ba 0 1) 1)
         (zerop (sbit ba 0 2))
         (eql (sbit ba 1 0) 1)
         (zerop (sbit ba 1 1))
         (eql (sbit ba 1 2) 1)
         (zerop (sbit ba 2 0))
         (eql (sbit ba 2 1) 1)
         (zerop (sbit ba 2 2))))))
(define-test sacla-must-array.137 (:tag :sacla)
 (assert-true
  (let ((ba (make-array '(3 3 3) :element-type 'bit)))
    (dotimes (i (* 3 3 3))
      (setf (bit ba (floor i 9) (floor (mod i 9) 3) (mod i 3))
              (if (evenp i) 0 1)))
    (dotimes (i (* 3 3 3) t)
      (unless (eql (row-major-aref ba i) (if (evenp i) 0 1))
        (return nil))))))
(define-test sacla-must-array.138 (:tag :sacla)
 (assert-true
  (let ((ba (make-array '(3 3 3) :element-type 'bit)))
    (dotimes (i (* 3 3 3))
      (setf (sbit ba (floor i 9) (floor (mod i 9) 3) (mod i 3))
              (if (evenp i) 0 1)))
    (dotimes (i (* 3 3 3) t)
      (unless (eql (row-major-aref ba i) (if (evenp i) 0 1))
        (return nil))))))
(define-test sacla-must-array.139 (:tag :sacla)
 (assert-true
  (let ((ba (make-array '(1 2 3 4 5) :element-type 'bit)))
    (dotimes (i (* 1 2 3 4 5))
      (setf (bit ba
                 (floor i (* 1 2 3 4 5))
                 (floor (mod i (* 2 3 4 5)) (* 3 4 5))
                 (floor (mod i (* 3 4 5)) (* 4 5))
                 (floor (mod i (* 4 5)) 5)
                 (mod i 5))
              (if (evenp i) 0 1)))
    (dotimes (i (* 1 2 3 4 5) t)
      (unless (eql (row-major-aref ba i) (if (evenp i) 0 1))
        (return nil))))))
(define-test sacla-must-array.140 (:tag :sacla)
 (assert-true
  (let ((ba (make-array '(1 2 3 4 5) :element-type 'bit)))
    (dotimes (i (* 1 2 3 4 5))
      (setf (sbit ba
                  (floor i (* 1 2 3 4 5))
                  (floor (mod i (* 2 3 4 5)) (* 3 4 5))
                  (floor (mod i (* 3 4 5)) (* 4 5))
                  (floor (mod i (* 4 5)) 5)
                  (mod i 5))
              (if (evenp i) 0 1)))
    (dotimes (i (* 1 2 3 4 5) t)
      (unless (eql (row-major-aref ba i) (if (evenp i) 0 1))
        (return nil))))))
(define-test sacla-must-array.141 (:tag :sacla)
 (assert-true
  (let ((ba (make-array 8 :element-type 'bit :initial-element 1)))
    (and (eql (setf (bit ba 3) 0) 0)
         (eql (bit ba 3) 0)
         (eql (sbit ba 5) 1)
         (eql (setf (sbit ba 5) 0) 0)
         (eql (sbit ba 5) 0)))))
(define-test sacla-must-array.142 (:tag :sacla)
 (assert-true
  (let ((ba (make-array 10 :element-type 'bit :fill-pointer 0)))
    (dotimes (i 10) (vector-push (if (oddp i) 0 1) ba))
    (dotimes (i 10 t)
      (unless
          (and (eql (bit ba i) (if (oddp i) 0 1))
               (or (not (simple-vector-p ba))
                   (eql (sbit ba i) (if (oddp i) 0 1)))
               (eql (aref ba i) (if (oddp i) 0 1)))
        (return nil))))))
(define-test sacla-must-array.143 (:tag :sacla)
 (assert-true
  (let ((ba (make-array 10 :element-type 'bit :fill-pointer 0)))
    (dotimes (i 10) (vector-push (if (oddp i) 0 1) ba))
    (dotimes (j 10 t)
      (let ((i (- 9 j)))
        (unless
            (and (eql (bit ba i) (if (oddp i) 0 1))
                 (or (not (simple-vector-p ba))
                     (eql (sbit ba i) (if (oddp i) 0 1)))
                 (eql (aref ba i) (if (oddp i) 0 1))
                 (eql (vector-pop ba) (if (oddp i) 0 1)))
          (return nil)))))))
(define-test sacla-must-array.144 (:tag :sacla)
 (assert-true (equal (bit-and #*11101010 #*01101011) #*01101010)))
(define-test sacla-must-array.145 (:tag :sacla)
 (assert-true (equal (bit-and #*11101010 #*01101011 nil) #*01101010)))
(define-test sacla-must-array.146 (:tag :sacla)
 (assert-true
  (equal
   (bit-and (make-array 8 :element-type 'bit :initial-contents #*11101010)
            #*01101011
            t)
   #*01101010)))
(define-test sacla-must-array.147 (:tag :sacla)
 (assert-true
  (equal (bit-and #*11101010 #*01101011 (make-array 8 :element-type 'bit))
         #*01101010)))
(define-test sacla-must-array.148 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*11101010))
         (ba2 (make-array 8 :element-type 'bit :initial-contents #*01101011))
         (ba (bit-and ba1 ba2)))
    (and (not (eq ba1 ba))
         (not (eq ba2 ba))
         (not (eq ba1 ba2))
         (equal ba #*01101010)))))
(define-test sacla-must-array.149 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01010101))
         (ba (bit-and ba1 #*10101010 t)))
    (and (eq ba1 ba) (equal ba1 #*00000000)))))
(define-test sacla-must-array.150 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
         (ba (bit-and ba1 #*00111110 t)))
    (and (eq ba1 ba) (equal ba1 #*00110000)))))
(define-test sacla-must-array.151 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
         (ba2 (make-array 8 :element-type 'bit :initial-contents #*00111110))
         (ba3 (make-array 8 :element-type 'bit))
         (ba4 (bit-and ba1 ba2 ba3)))
    (and (eq ba3 ba4)
         (equal ba3 #*00110000)
         (not (eq ba1 ba3))
         (not (eq ba1 ba4))
         (not (eq ba2 ba3))
         (not (eq ba2 ba4))))))
(define-test sacla-must-array.152 (:tag :sacla)
 (assert-true
  (equalp
   (bit-and
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010)))
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*000 #*000)))))
(define-test sacla-must-array.153 (:tag :sacla)
 (assert-true
  (equalp
   (bit-and
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010))
    nil)
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*000 #*000)))))
(define-test sacla-must-array.154 (:tag :sacla)
 (assert-true
  (equalp
   (bit-and
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010))
    t)
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*000 #*000)))))
(define-test sacla-must-array.155 (:tag :sacla)
 (assert-true
  (equalp
   (bit-and
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010))
    (make-array '(2 3) :element-type 'bit))
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*000 #*000)))))
(define-test sacla-must-array.156 (:tag :sacla)
 (assert-true
  (let* ((ba1
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*010 #*101)))
         (ba2
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*101 #*010)))
         (ba (bit-and ba1 ba2)))
    (and (not (eq ba1 ba))
         (not (eq ba2 ba))
         (not (eq ba1 ba2))
         (equalp ba
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*000 #*000)))))))
(define-test sacla-must-array.157 (:tag :sacla)
 (assert-true
  (let* ((ba1
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*010 #*101)))
         (ba2
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*101 #*010)))
         (ba (bit-and ba1 ba2 t)))
    (and (eq ba1 ba)
         (equalp ba1
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*000 #*000)))))))
(define-test sacla-must-array.158 (:tag :sacla)
 (assert-true
  (let* ((ba1
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*010 #*101)))
         (ba2
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*101 #*010)))
         (ba3 (make-array '(2 3) :element-type 'bit))
         (ba4 (bit-and ba1 ba2 ba3)))
    (and (eq ba3 ba4)
         (equalp ba3
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*000 #*000)))
         (not (eq ba1 ba3))
         (not (eq ba1 ba4))
         (not (eq ba2 ba3))
         (not (eq ba2 ba4))))))
(define-test sacla-must-array.159 (:tag :sacla)
 (assert-true (equal (bit-andc1 #*11101010 #*01101011) #*00000001)))
(define-test sacla-must-array.160 (:tag :sacla)
 (assert-true (equal (bit-andc1 #*11101010 #*01101011 nil) #*00000001)))
(define-test sacla-must-array.161 (:tag :sacla)
 (assert-true
  (equal
   (bit-andc1 (make-array 8 :element-type 'bit :initial-contents #*11101010)
              #*01101011
              t)
   #*00000001)))
(define-test sacla-must-array.162 (:tag :sacla)
 (assert-true
  (equal (bit-andc1 #*11101010 #*01101011 (make-array 8 :element-type 'bit))
         #*00000001)))
(define-test sacla-must-array.163 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*11101010))
         (ba2 (make-array 8 :element-type 'bit :initial-contents #*01101011))
         (ba (bit-andc1 ba1 ba2)))
    (and (not (eq ba1 ba))
         (not (eq ba2 ba))
         (not (eq ba1 ba2))
         (equal ba #*00000001)))))
(define-test sacla-must-array.164 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01010101))
         (ba (bit-andc1 ba1 #*10101010 t)))
    (and (eq ba1 ba) (equal ba1 #*10101010)))))
(define-test sacla-must-array.165 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
         (ba (bit-andc1 ba1 #*00111110 t)))
    (and (eq ba1 ba) (equal ba1 #*00001110)))))
(define-test sacla-must-array.166 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
         (ba2 (make-array 8 :element-type 'bit :initial-contents #*00111110))
         (ba3 (make-array 8 :element-type 'bit))
         (ba4 (bit-andc1 ba1 ba2 ba3)))
    (and (eq ba3 ba4)
         (equal ba3 #*00001110)
         (not (eq ba1 ba3))
         (not (eq ba1 ba4))
         (not (eq ba2 ba3))
         (not (eq ba2 ba4))))))
(define-test sacla-must-array.167 (:tag :sacla)
 (assert-true
  (equalp
   (bit-andc1
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010)))
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010)))))
(define-test sacla-must-array.168 (:tag :sacla)
 (assert-true
  (equalp
   (bit-andc1
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010))
    nil)
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010)))))
(define-test sacla-must-array.169 (:tag :sacla)
 (assert-true
  (equalp
   (bit-andc1
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010))
    t)
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010)))))
(define-test sacla-must-array.170 (:tag :sacla)
 (assert-true
  (equalp
   (bit-andc1
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010))
    (make-array '(2 3) :element-type 'bit))
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010)))))
(define-test sacla-must-array.171 (:tag :sacla)
 (assert-true
  (let* ((ba1
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*010 #*101)))
         (ba2
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*101 #*010)))
         (ba (bit-andc1 ba1 ba2)))
    (and (not (eq ba1 ba))
         (not (eq ba2 ba))
         (not (eq ba1 ba2))
         (equalp ba
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*101 #*010)))))))
(define-test sacla-must-array.172 (:tag :sacla)
 (assert-true
  (let* ((ba1
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*010 #*101)))
         (ba2
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*101 #*010)))
         (ba (bit-andc1 ba1 ba2 t)))
    (and (eq ba1 ba)
         (equalp ba1
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*101 #*010)))))))
(define-test sacla-must-array.173 (:tag :sacla)
 (assert-true
  (let* ((ba1
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*010 #*101)))
         (ba2
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*101 #*010)))
         (ba3 (make-array '(2 3) :element-type 'bit))
         (ba4 (bit-andc1 ba1 ba2 ba3)))
    (and (eq ba3 ba4)
         (equalp ba3
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*101 #*010)))
         (not (eq ba1 ba3))
         (not (eq ba1 ba4))
         (not (eq ba2 ba3))
         (not (eq ba2 ba4))))))
(define-test sacla-must-array.174 (:tag :sacla)
 (assert-true (equal (bit-andc2 #*11101010 #*01101011) #*10000000)))
(define-test sacla-must-array.175 (:tag :sacla)
 (assert-true (equal (bit-andc2 #*11101010 #*01101011 nil) #*10000000)))
(define-test sacla-must-array.176 (:tag :sacla)
 (assert-true
  (equal
   (bit-andc2 (make-array 8 :element-type 'bit :initial-contents #*11101010)
              #*01101011
              t)
   #*10000000)))
(define-test sacla-must-array.177 (:tag :sacla)
 (assert-true
  (equal (bit-andc2 #*11101010 #*01101011 (make-array 8 :element-type 'bit))
         #*10000000)))
(define-test sacla-must-array.178 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*11101010))
         (ba2 (make-array 8 :element-type 'bit :initial-contents #*01101011))
         (ba (bit-andc2 ba1 ba2)))
    (and (not (eq ba1 ba))
         (not (eq ba2 ba))
         (not (eq ba1 ba2))
         (equal ba #*10000000)))))
(define-test sacla-must-array.179 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01010101))
         (ba (bit-andc2 ba1 #*10101010 t)))
    (and (eq ba1 ba) (equal ba1 #*01010101)))))
(define-test sacla-must-array.180 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
         (ba (bit-andc2 ba1 #*00111110 t)))
    (and (eq ba1 ba) (equal ba1 #*01000001)))))
(define-test sacla-must-array.181 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
         (ba2 (make-array 8 :element-type 'bit :initial-contents #*00111110))
         (ba3 (make-array 8 :element-type 'bit))
         (ba4 (bit-andc2 ba1 ba2 ba3)))
    (and (eq ba3 ba4)
         (equal ba3 #*01000001)
         (not (eq ba1 ba3))
         (not (eq ba1 ba4))
         (not (eq ba2 ba3))
         (not (eq ba2 ba4))))))
(define-test sacla-must-array.182 (:tag :sacla)
 (assert-true
  (equalp
   (bit-andc2
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010)))
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101)))))
(define-test sacla-must-array.183 (:tag :sacla)
 (assert-true
  (equalp
   (bit-andc2
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010))
    nil)
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101)))))
(define-test sacla-must-array.184 (:tag :sacla)
 (assert-true
  (equalp
   (bit-andc2
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010))
    t)
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101)))))
(define-test sacla-must-array.185 (:tag :sacla)
 (assert-true
  (equalp
   (bit-andc2
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010))
    (make-array '(2 3) :element-type 'bit))
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101)))))
(define-test sacla-must-array.186 (:tag :sacla)
 (assert-true
  (let* ((ba1
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*010 #*101)))
         (ba2
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*101 #*010)))
         (ba (bit-andc2 ba1 ba2)))
    (and (not (eq ba1 ba))
         (not (eq ba2 ba))
         (not (eq ba1 ba2))
         (equalp ba
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*010 #*101)))))))
(define-test sacla-must-array.187 (:tag :sacla)
 (assert-true
  (let* ((ba1
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*010 #*101)))
         (ba2
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*101 #*010)))
         (ba (bit-andc2 ba1 ba2 t)))
    (and (eq ba1 ba)
         (equalp ba1
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*010 #*101)))))))
(define-test sacla-must-array.188 (:tag :sacla)
 (assert-true
  (let* ((ba1
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*010 #*101)))
         (ba2
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*101 #*010)))
         (ba3 (make-array '(2 3) :element-type 'bit))
         (ba4 (bit-andc2 ba1 ba2 ba3)))
    (and (eq ba3 ba4)
         (equalp ba3
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*010 #*101)))
         (not (eq ba1 ba3))
         (not (eq ba1 ba4))
         (not (eq ba2 ba3))
         (not (eq ba2 ba4))))))
(define-test sacla-must-array.189 (:tag :sacla)
 (assert-true (equal (bit-eqv #*11101010 #*01101011) #*01111110)))
(define-test sacla-must-array.190 (:tag :sacla)
 (assert-true (equal (bit-eqv #*11101010 #*01101011 nil) #*01111110)))
(define-test sacla-must-array.191 (:tag :sacla)
 (assert-true
  (equal
   (bit-eqv (make-array 8 :element-type 'bit :initial-contents #*11101010)
            #*01101011
            t)
   #*01111110)))
(define-test sacla-must-array.192 (:tag :sacla)
 (assert-true
  (equal (bit-eqv #*11101010 #*01101011 (make-array 8 :element-type 'bit))
         #*01111110)))
(define-test sacla-must-array.193 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*11101010))
         (ba2 (make-array 8 :element-type 'bit :initial-contents #*01101011))
         (ba (bit-eqv ba1 ba2)))
    (and (not (eq ba1 ba))
         (not (eq ba2 ba))
         (not (eq ba1 ba2))
         (equal ba #*01111110)))))
(define-test sacla-must-array.194 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01010101))
         (ba (bit-eqv ba1 #*10101010 t)))
    (and (eq ba1 ba) (equal ba1 #*00000000)))))
(define-test sacla-must-array.195 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
         (ba (bit-eqv ba1 #*00111110 t)))
    (and (eq ba1 ba) (equal ba1 #*10110000)))))
(define-test sacla-must-array.196 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
         (ba2 (make-array 8 :element-type 'bit :initial-contents #*00111110))
         (ba3 (make-array 8 :element-type 'bit))
         (ba4 (bit-eqv ba1 ba2 ba3)))
    (and (eq ba3 ba4)
         (equal ba3 #*10110000)
         (not (eq ba1 ba3))
         (not (eq ba1 ba4))
         (not (eq ba2 ba3))
         (not (eq ba2 ba4))))))
(define-test sacla-must-array.197 (:tag :sacla)
 (assert-true
  (equalp
   (bit-eqv
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010)))
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*000 #*000)))))
(define-test sacla-must-array.198 (:tag :sacla)
 (assert-true
  (equalp
   (bit-eqv
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010))
    nil)
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*000 #*000)))))
(define-test sacla-must-array.199 (:tag :sacla)
 (assert-true
  (equalp
   (bit-eqv
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010))
    t)
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*000 #*000)))))
(define-test sacla-must-array.200 (:tag :sacla)
 (assert-true
  (equalp
   (bit-eqv
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010))
    (make-array '(2 3) :element-type 'bit))
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*000 #*000)))))
(define-test sacla-must-array.201 (:tag :sacla)
 (assert-true
  (let* ((ba1
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*010 #*101)))
         (ba2
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*101 #*010)))
         (ba (bit-eqv ba1 ba2)))
    (and (not (eq ba1 ba))
         (not (eq ba2 ba))
         (not (eq ba1 ba2))
         (equalp ba
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*000 #*000)))))))
(define-test sacla-must-array.202 (:tag :sacla)
 (assert-true
  (let* ((ba1
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*010 #*101)))
         (ba2
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*101 #*010)))
         (ba (bit-eqv ba1 ba2 t)))
    (and (eq ba1 ba)
         (equalp ba1
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*000 #*000)))))))
(define-test sacla-must-array.203 (:tag :sacla)
 (assert-true
  (let* ((ba1
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*010 #*101)))
         (ba2
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*101 #*010)))
         (ba3 (make-array '(2 3) :element-type 'bit))
         (ba4 (bit-eqv ba1 ba2 ba3)))
    (and (eq ba3 ba4)
         (equalp ba3
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*000 #*000)))
         (not (eq ba1 ba3))
         (not (eq ba1 ba4))
         (not (eq ba2 ba3))
         (not (eq ba2 ba4))))))
(define-test sacla-must-array.204 (:tag :sacla)
 (assert-true (equal (bit-ior #*11101010 #*01101011) #*11101011)))
(define-test sacla-must-array.205 (:tag :sacla)
 (assert-true (equal (bit-ior #*11101010 #*01101011 nil) #*11101011)))
(define-test sacla-must-array.206 (:tag :sacla)
 (assert-true
  (equal
   (bit-ior (make-array 8 :element-type 'bit :initial-contents #*11101010)
            #*01101011
            t)
   #*11101011)))
(define-test sacla-must-array.207 (:tag :sacla)
 (assert-true
  (equal (bit-ior #*11101010 #*01101011 (make-array 8 :element-type 'bit))
         #*11101011)))
(define-test sacla-must-array.208 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*11101010))
         (ba2 (make-array 8 :element-type 'bit :initial-contents #*01101011))
         (ba (bit-ior ba1 ba2)))
    (and (not (eq ba1 ba))
         (not (eq ba2 ba))
         (not (eq ba1 ba2))
         (equal ba #*11101011)))))
(define-test sacla-must-array.209 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01010101))
         (ba (bit-ior ba1 #*10101010 t)))
    (and (eq ba1 ba) (equal ba1 #*11111111)))))
(define-test sacla-must-array.210 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
         (ba (bit-ior ba1 #*00111110 t)))
    (and (eq ba1 ba) (equal ba1 #*01111111)))))
(define-test sacla-must-array.211 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
         (ba2 (make-array 8 :element-type 'bit :initial-contents #*00111110))
         (ba3 (make-array 8 :element-type 'bit))
         (ba4 (bit-ior ba1 ba2 ba3)))
    (and (eq ba3 ba4)
         (equal ba3 #*01111111)
         (not (eq ba1 ba3))
         (not (eq ba1 ba4))
         (not (eq ba2 ba3))
         (not (eq ba2 ba4))))))
(define-test sacla-must-array.212 (:tag :sacla)
 (assert-true
  (equalp
   (bit-ior
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010)))
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*111 #*111)))))
(define-test sacla-must-array.213 (:tag :sacla)
 (assert-true
  (equalp
   (bit-ior
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010))
    nil)
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*111 #*111)))))
(define-test sacla-must-array.214 (:tag :sacla)
 (assert-true
  (equalp
   (bit-ior
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010))
    t)
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*111 #*111)))))
(define-test sacla-must-array.215 (:tag :sacla)
 (assert-true
  (equalp
   (bit-ior
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010))
    (make-array '(2 3) :element-type 'bit))
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*111 #*111)))))
(define-test sacla-must-array.216 (:tag :sacla)
 (assert-true
  (let* ((ba1
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*010 #*101)))
         (ba2
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*101 #*010)))
         (ba (bit-ior ba1 ba2)))
    (and (not (eq ba1 ba))
         (not (eq ba2 ba))
         (not (eq ba1 ba2))
         (equalp ba
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*111 #*111)))))))
(define-test sacla-must-array.217 (:tag :sacla)
 (assert-true
  (let* ((ba1
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*010 #*101)))
         (ba2
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*101 #*010)))
         (ba (bit-ior ba1 ba2 t)))
    (and (eq ba1 ba)
         (equalp ba1
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*111 #*111)))))))
(define-test sacla-must-array.218 (:tag :sacla)
 (assert-true
  (let* ((ba1
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*010 #*101)))
         (ba2
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*101 #*010)))
         (ba3 (make-array '(2 3) :element-type 'bit))
         (ba4 (bit-ior ba1 ba2 ba3)))
    (and (eq ba3 ba4)
         (equalp ba3
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*111 #*111)))
         (not (eq ba1 ba3))
         (not (eq ba1 ba4))
         (not (eq ba2 ba3))
         (not (eq ba2 ba4))))))
(define-test sacla-must-array.219 (:tag :sacla)
 (assert-true (equal (bit-nand #*11101010 #*01101011) #*10010101)))
(define-test sacla-must-array.220 (:tag :sacla)
 (assert-true (equal (bit-nand #*11101010 #*01101011 nil) #*10010101)))
(define-test sacla-must-array.221 (:tag :sacla)
 (assert-true
  (equal
   (bit-nand (make-array 8 :element-type 'bit :initial-contents #*11101010)
             #*01101011
             t)
   #*10010101)))
(define-test sacla-must-array.222 (:tag :sacla)
 (assert-true
  (equal (bit-nand #*11101010 #*01101011 (make-array 8 :element-type 'bit))
         #*10010101)))
(define-test sacla-must-array.223 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*11101010))
         (ba2 (make-array 8 :element-type 'bit :initial-contents #*01101011))
         (ba (bit-nand ba1 ba2)))
    (and (not (eq ba1 ba))
         (not (eq ba2 ba))
         (not (eq ba1 ba2))
         (equal ba #*10010101)))))
(define-test sacla-must-array.224 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01010101))
         (ba (bit-nand ba1 #*10101010 t)))
    (and (eq ba1 ba) (equal ba1 #*11111111)))))
(define-test sacla-must-array.225 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
         (ba (bit-nand ba1 #*00111110 t)))
    (and (eq ba1 ba) (equal ba1 #*11001111)))))
(define-test sacla-must-array.226 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
         (ba2 (make-array 8 :element-type 'bit :initial-contents #*00111110))
         (ba3 (make-array 8 :element-type 'bit))
         (ba4 (bit-nand ba1 ba2 ba3)))
    (and (eq ba3 ba4)
         (equal ba3 #*11001111)
         (not (eq ba1 ba3))
         (not (eq ba1 ba4))
         (not (eq ba2 ba3))
         (not (eq ba2 ba4))))))
(define-test sacla-must-array.227 (:tag :sacla)
 (assert-true
  (equalp
   (bit-nand
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010)))
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*111 #*111)))))
(define-test sacla-must-array.228 (:tag :sacla)
 (assert-true
  (equalp
   (bit-nand
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010))
    nil)
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*111 #*111)))))
(define-test sacla-must-array.229 (:tag :sacla)
 (assert-true
  (equalp
   (bit-nand
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010))
    t)
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*111 #*111)))))
(define-test sacla-must-array.230 (:tag :sacla)
 (assert-true
  (equalp
   (bit-nand
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010))
    (make-array '(2 3) :element-type 'bit))
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*111 #*111)))))
(define-test sacla-must-array.231 (:tag :sacla)
 (assert-true
  (let* ((ba1
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*010 #*101)))
         (ba2
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*101 #*010)))
         (ba (bit-nand ba1 ba2)))
    (and (not (eq ba1 ba))
         (not (eq ba2 ba))
         (not (eq ba1 ba2))
         (equalp ba
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*111 #*111)))))))
(define-test sacla-must-array.232 (:tag :sacla)
 (assert-true
  (let* ((ba1
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*010 #*101)))
         (ba2
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*101 #*010)))
         (ba (bit-nand ba1 ba2 t)))
    (and (eq ba1 ba)
         (equalp ba1
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*111 #*111)))))))
(define-test sacla-must-array.233 (:tag :sacla)
 (assert-true
  (let* ((ba1
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*010 #*101)))
         (ba2
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*101 #*010)))
         (ba3 (make-array '(2 3) :element-type 'bit))
         (ba4 (bit-nand ba1 ba2 ba3)))
    (and (eq ba3 ba4)
         (equalp ba3
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*111 #*111)))
         (not (eq ba1 ba3))
         (not (eq ba1 ba4))
         (not (eq ba2 ba3))
         (not (eq ba2 ba4))))))
(define-test sacla-must-array.234 (:tag :sacla)
 (assert-true (equal (bit-nor #*11101010 #*01101011) #*00010100)))
(define-test sacla-must-array.235 (:tag :sacla)
 (assert-true (equal (bit-nor #*11101010 #*01101011 nil) #*00010100)))
(define-test sacla-must-array.236 (:tag :sacla)
 (assert-true
  (equal
   (bit-nor (make-array 8 :element-type 'bit :initial-contents #*11101010)
            #*01101011
            t)
   #*00010100)))
(define-test sacla-must-array.237 (:tag :sacla)
 (assert-true
  (equal (bit-nor #*11101010 #*01101011 (make-array 8 :element-type 'bit))
         #*00010100)))
(define-test sacla-must-array.238 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*11101010))
         (ba2 (make-array 8 :element-type 'bit :initial-contents #*01101011))
         (ba (bit-nor ba1 ba2)))
    (and (not (eq ba1 ba))
         (not (eq ba2 ba))
         (not (eq ba1 ba2))
         (equal ba #*00010100)))))
(define-test sacla-must-array.239 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01010101))
         (ba (bit-nor ba1 #*10101010 t)))
    (and (eq ba1 ba) (equal ba1 #*00000000)))))
(define-test sacla-must-array.240 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
         (ba (bit-nor ba1 #*00111110 t)))
    (and (eq ba1 ba) (equal ba1 #*10000000)))))
(define-test sacla-must-array.241 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
         (ba2 (make-array 8 :element-type 'bit :initial-contents #*00111110))
         (ba3 (make-array 8 :element-type 'bit))
         (ba4 (bit-nor ba1 ba2 ba3)))
    (and (eq ba3 ba4)
         (equal ba3 #*10000000)
         (not (eq ba1 ba3))
         (not (eq ba1 ba4))
         (not (eq ba2 ba3))
         (not (eq ba2 ba4))))))
(define-test sacla-must-array.242 (:tag :sacla)
 (assert-true
  (equalp
   (bit-nor
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010)))
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*000 #*000)))))
(define-test sacla-must-array.243 (:tag :sacla)
 (assert-true
  (equalp
   (bit-nor
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010))
    nil)
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*000 #*000)))))
(define-test sacla-must-array.244 (:tag :sacla)
 (assert-true
  (equalp
   (bit-nor
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010))
    t)
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*000 #*000)))))
(define-test sacla-must-array.245 (:tag :sacla)
 (assert-true
  (equalp
   (bit-nor
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010))
    (make-array '(2 3) :element-type 'bit))
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*000 #*000)))))
(define-test sacla-must-array.246 (:tag :sacla)
 (assert-true
  (let* ((ba1
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*010 #*101)))
         (ba2
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*101 #*010)))
         (ba (bit-nor ba1 ba2)))
    (and (not (eq ba1 ba))
         (not (eq ba2 ba))
         (not (eq ba1 ba2))
         (equalp ba
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*000 #*000)))))))
(define-test sacla-must-array.247 (:tag :sacla)
 (assert-true
  (let* ((ba1
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*010 #*101)))
         (ba2
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*101 #*010)))
         (ba (bit-nor ba1 ba2 t)))
    (and (eq ba1 ba)
         (equalp ba1
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*000 #*000)))))))
(define-test sacla-must-array.248 (:tag :sacla)
 (assert-true
  (let* ((ba1
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*010 #*101)))
         (ba2
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*101 #*010)))
         (ba3 (make-array '(2 3) :element-type 'bit))
         (ba4 (bit-nor ba1 ba2 ba3)))
    (and (eq ba3 ba4)
         (equalp ba3
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*000 #*000)))
         (not (eq ba1 ba3))
         (not (eq ba1 ba4))
         (not (eq ba2 ba3))
         (not (eq ba2 ba4))))))
(define-test sacla-must-array.249 (:tag :sacla)
 (assert-true (equal (bit-orc1 #*11101010 #*01101011) #*01111111)))
(define-test sacla-must-array.250 (:tag :sacla)
 (assert-true (equal (bit-orc1 #*11101010 #*01101011 nil) #*01111111)))
(define-test sacla-must-array.251 (:tag :sacla)
 (assert-true
  (equal
   (bit-orc1 (make-array 8 :element-type 'bit :initial-contents #*11101010)
             #*01101011
             t)
   #*01111111)))
(define-test sacla-must-array.252 (:tag :sacla)
 (assert-true
  (equal (bit-orc1 #*11101010 #*01101011 (make-array 8 :element-type 'bit))
         #*01111111)))
(define-test sacla-must-array.253 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*11101010))
         (ba2 (make-array 8 :element-type 'bit :initial-contents #*01101011))
         (ba (bit-orc1 ba1 ba2)))
    (and (not (eq ba1 ba))
         (not (eq ba2 ba))
         (not (eq ba1 ba2))
         (equal ba #*01111111)))))
(define-test sacla-must-array.254 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01010101))
         (ba (bit-orc1 ba1 #*10101010 t)))
    (and (eq ba1 ba) (equal ba1 #*10101010)))))
(define-test sacla-must-array.255 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
         (ba (bit-orc1 ba1 #*00111110 t)))
    (and (eq ba1 ba) (equal ba1 #*10111110)))))
(define-test sacla-must-array.256 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
         (ba2 (make-array 8 :element-type 'bit :initial-contents #*00111110))
         (ba3 (make-array 8 :element-type 'bit))
         (ba4 (bit-orc1 ba1 ba2 ba3)))
    (and (eq ba3 ba4)
         (equal ba3 #*10111110)
         (not (eq ba1 ba3))
         (not (eq ba1 ba4))
         (not (eq ba2 ba3))
         (not (eq ba2 ba4))))))
(define-test sacla-must-array.257 (:tag :sacla)
 (assert-true
  (equalp
   (bit-orc1
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010)))
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010)))))
(define-test sacla-must-array.258 (:tag :sacla)
 (assert-true
  (equalp
   (bit-orc1
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010))
    nil)
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010)))))
(define-test sacla-must-array.259 (:tag :sacla)
 (assert-true
  (equalp
   (bit-orc1
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010))
    t)
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010)))))
(define-test sacla-must-array.260 (:tag :sacla)
 (assert-true
  (equalp
   (bit-orc1
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010))
    (make-array '(2 3) :element-type 'bit))
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010)))))
(define-test sacla-must-array.261 (:tag :sacla)
 (assert-true
  (let* ((ba1
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*010 #*101)))
         (ba2
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*101 #*010)))
         (ba (bit-orc1 ba1 ba2)))
    (and (not (eq ba1 ba))
         (not (eq ba2 ba))
         (not (eq ba1 ba2))
         (equalp ba
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*101 #*010)))))))
(define-test sacla-must-array.262 (:tag :sacla)
 (assert-true
  (let* ((ba1
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*010 #*101)))
         (ba2
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*101 #*010)))
         (ba (bit-orc1 ba1 ba2 t)))
    (and (eq ba1 ba)
         (equalp ba1
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*101 #*010)))))))
(define-test sacla-must-array.263 (:tag :sacla)
 (assert-true
  (let* ((ba1
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*010 #*101)))
         (ba2
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*101 #*010)))
         (ba3 (make-array '(2 3) :element-type 'bit))
         (ba4 (bit-orc1 ba1 ba2 ba3)))
    (and (eq ba3 ba4)
         (equalp ba3
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*101 #*010)))
         (not (eq ba1 ba3))
         (not (eq ba1 ba4))
         (not (eq ba2 ba3))
         (not (eq ba2 ba4))))))
(define-test sacla-must-array.264 (:tag :sacla)
 (assert-true (equal (bit-orc2 #*11101010 #*01101011) #*11111110)))
(define-test sacla-must-array.265 (:tag :sacla)
 (assert-true (equal (bit-orc2 #*11101010 #*01101011 nil) #*11111110)))
(define-test sacla-must-array.266 (:tag :sacla)
 (assert-true
  (equal
   (bit-orc2 (make-array 8 :element-type 'bit :initial-contents #*11101010)
             #*01101011
             t)
   #*11111110)))
(define-test sacla-must-array.267 (:tag :sacla)
 (assert-true
  (equal (bit-orc2 #*11101010 #*01101011 (make-array 8 :element-type 'bit))
         #*11111110)))
(define-test sacla-must-array.268 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*11101010))
         (ba2 (make-array 8 :element-type 'bit :initial-contents #*01101011))
         (ba (bit-orc2 ba1 ba2)))
    (and (not (eq ba1 ba))
         (not (eq ba2 ba))
         (not (eq ba1 ba2))
         (equal ba #*11111110)))))
(define-test sacla-must-array.269 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01010101))
         (ba (bit-orc2 ba1 #*10101010 t)))
    (and (eq ba1 ba) (equal ba1 #*01010101)))))
(define-test sacla-must-array.270 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
         (ba (bit-orc2 ba1 #*00111110 t)))
    (and (eq ba1 ba) (equal ba1 #*11110001)))))
(define-test sacla-must-array.271 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
         (ba2 (make-array 8 :element-type 'bit :initial-contents #*00111110))
         (ba3 (make-array 8 :element-type 'bit))
         (ba4 (bit-orc2 ba1 ba2 ba3)))
    (and (eq ba3 ba4)
         (equal ba3 #*11110001)
         (not (eq ba1 ba3))
         (not (eq ba1 ba4))
         (not (eq ba2 ba3))
         (not (eq ba2 ba4))))))
(define-test sacla-must-array.272 (:tag :sacla)
 (assert-true
  (equalp
   (bit-orc2
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010)))
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101)))))
(define-test sacla-must-array.273 (:tag :sacla)
 (assert-true
  (equalp
   (bit-orc2
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010))
    nil)
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101)))))
(define-test sacla-must-array.274 (:tag :sacla)
 (assert-true
  (equalp
   (bit-orc2
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010))
    t)
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101)))))
(define-test sacla-must-array.275 (:tag :sacla)
 (assert-true
  (equalp
   (bit-orc2
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010))
    (make-array '(2 3) :element-type 'bit))
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101)))))
(define-test sacla-must-array.276 (:tag :sacla)
 (assert-true
  (let* ((ba1
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*010 #*101)))
         (ba2
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*101 #*010)))
         (ba (bit-orc2 ba1 ba2)))
    (and (not (eq ba1 ba))
         (not (eq ba2 ba))
         (not (eq ba1 ba2))
         (equalp ba
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*010 #*101)))))))
(define-test sacla-must-array.277 (:tag :sacla)
 (assert-true
  (let* ((ba1
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*010 #*101)))
         (ba2
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*101 #*010)))
         (ba (bit-orc2 ba1 ba2 t)))
    (and (eq ba1 ba)
         (equalp ba1
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*010 #*101)))))))
(define-test sacla-must-array.278 (:tag :sacla)
 (assert-true
  (let* ((ba1
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*010 #*101)))
         (ba2
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*101 #*010)))
         (ba3 (make-array '(2 3) :element-type 'bit))
         (ba4 (bit-orc2 ba1 ba2 ba3)))
    (and (eq ba3 ba4)
         (equalp ba3
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*010 #*101)))
         (not (eq ba1 ba3))
         (not (eq ba1 ba4))
         (not (eq ba2 ba3))
         (not (eq ba2 ba4))))))
(define-test sacla-must-array.279 (:tag :sacla)
 (assert-true (equal (bit-xor #*11101010 #*01101011) #*10000001)))
(define-test sacla-must-array.280 (:tag :sacla)
 (assert-true (equal (bit-xor #*11101010 #*01101011 nil) #*10000001)))
(define-test sacla-must-array.281 (:tag :sacla)
 (assert-true
  (equal
   (bit-xor (make-array 8 :element-type 'bit :initial-contents #*11101010)
            #*01101011
            t)
   #*10000001)))
(define-test sacla-must-array.282 (:tag :sacla)
 (assert-true
  (equal (bit-xor #*11101010 #*01101011 (make-array 8 :element-type 'bit))
         #*10000001)))
(define-test sacla-must-array.283 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*11101010))
         (ba2 (make-array 8 :element-type 'bit :initial-contents #*01101011))
         (ba (bit-xor ba1 ba2)))
    (and (not (eq ba1 ba))
         (not (eq ba2 ba))
         (not (eq ba1 ba2))
         (equal ba #*10000001)))))
(define-test sacla-must-array.284 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01010101))
         (ba (bit-xor ba1 #*10101010 t)))
    (and (eq ba1 ba) (equal ba1 #*11111111)))))
(define-test sacla-must-array.285 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
         (ba (bit-xor ba1 #*00111110 t)))
    (and (eq ba1 ba) (equal ba1 #*01001111)))))
(define-test sacla-must-array.286 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
         (ba2 (make-array 8 :element-type 'bit :initial-contents #*00111110))
         (ba3 (make-array 8 :element-type 'bit))
         (ba4 (bit-xor ba1 ba2 ba3)))
    (and (eq ba3 ba4)
         (equal ba3 #*01001111)
         (not (eq ba1 ba3))
         (not (eq ba1 ba4))
         (not (eq ba2 ba3))
         (not (eq ba2 ba4))))))
(define-test sacla-must-array.287 (:tag :sacla)
 (assert-true
  (equalp
   (bit-xor
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010)))
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*111 #*111)))))
(define-test sacla-must-array.288 (:tag :sacla)
 (assert-true
  (equalp
   (bit-xor
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010))
    nil)
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*111 #*111)))))
(define-test sacla-must-array.289 (:tag :sacla)
 (assert-true
  (equalp
   (bit-xor
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010))
    t)
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*111 #*111)))))
(define-test sacla-must-array.290 (:tag :sacla)
 (assert-true
  (equalp
   (bit-xor
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010))
    (make-array '(2 3) :element-type 'bit))
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*111 #*111)))))
(define-test sacla-must-array.291 (:tag :sacla)
 (assert-true
  (let* ((ba1
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*010 #*101)))
         (ba2
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*101 #*010)))
         (ba (bit-xor ba1 ba2)))
    (and (not (eq ba1 ba))
         (not (eq ba2 ba))
         (not (eq ba1 ba2))
         (equalp ba
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*111 #*111)))))))
(define-test sacla-must-array.292 (:tag :sacla)
 (assert-true
  (let* ((ba1
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*010 #*101)))
         (ba2
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*101 #*010)))
         (ba (bit-xor ba1 ba2 t)))
    (and (eq ba1 ba)
         (equalp ba1
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*111 #*111)))))))
(define-test sacla-must-array.293 (:tag :sacla)
 (assert-true
  (let* ((ba1
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*010 #*101)))
         (ba2
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*101 #*010)))
         (ba3 (make-array '(2 3) :element-type 'bit))
         (ba4 (bit-xor ba1 ba2 ba3)))
    (and (eq ba3 ba4)
         (equalp ba3
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*111 #*111)))
         (not (eq ba1 ba3))
         (not (eq ba1 ba4))
         (not (eq ba2 ba3))
         (not (eq ba2 ba4))))))
(define-test sacla-must-array.294 (:tag :sacla)
 (assert-true (equal (bit-not #*11101010) #*00010101)))
(define-test sacla-must-array.295 (:tag :sacla)
 (assert-true (equal (bit-not #*11101010 nil) #*00010101)))
(define-test sacla-must-array.296 (:tag :sacla)
 (assert-true
  (equal
   (bit-not (make-array 8 :element-type 'bit :initial-contents #*11101010) t)
   #*00010101)))
(define-test sacla-must-array.297 (:tag :sacla)
 (assert-true
  (equal (bit-not #*11101010 (make-array 8 :element-type 'bit)) #*00010101)))
(define-test sacla-must-array.298 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*11101010))
         (ba (bit-not ba1)))
    (and (not (eq ba1 ba)) (equal ba #*00010101)))))
(define-test sacla-must-array.299 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01010101))
         (ba (bit-not ba1 t)))
    (and (eq ba1 ba) (equal ba1 #*10101010)))))
(define-test sacla-must-array.300 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
         (ba (bit-not ba1 t)))
    (and (eq ba1 ba) (equal ba1 #*10001110)))))
(define-test sacla-must-array.301 (:tag :sacla)
 (assert-true
  (let* ((ba1 (make-array 8 :element-type 'bit :initial-contents #*01110001))
         (ba2 (make-array 8 :element-type 'bit))
         (ba3 (bit-not ba1 ba2)))
    (and (eq ba2 ba3)
         (equal ba2 #*10001110)
         (not (eq ba1 ba2))
         (not (eq ba1 ba3))))))
(define-test sacla-must-array.302 (:tag :sacla)
 (assert-true
  (equalp
   (bit-not
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101)))
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010)))))
(define-test sacla-must-array.303 (:tag :sacla)
 (assert-true
  (equalp
   (bit-not
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    nil)
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010)))))
(define-test sacla-must-array.304 (:tag :sacla)
 (assert-true
  (equalp
   (bit-not
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    t)
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010)))))
(define-test sacla-must-array.305 (:tag :sacla)
 (assert-true
  (equalp
   (bit-not
    (make-array '(2 3) :element-type 'bit :initial-contents '(#*010 #*101))
    (make-array '(2 3) :element-type 'bit))
   (make-array '(2 3) :element-type 'bit :initial-contents '(#*101 #*010)))))
(define-test sacla-must-array.306 (:tag :sacla)
 (assert-true
  (let* ((ba1
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*010 #*101)))
         (ba (bit-not ba1)))
    (and (not (eq ba1 ba))
         (equalp ba
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*101 #*010)))))))
(define-test sacla-must-array.307 (:tag :sacla)
 (assert-true
  (let* ((ba1
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*010 #*101)))
         (ba (bit-not ba1 t)))
    (and (eq ba1 ba)
         (equalp ba1
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*101 #*010)))))))
(define-test sacla-must-array.308 (:tag :sacla)
 (assert-true
  (let* ((ba1
          (make-array '(2 3)
                      :element-type 'bit
                      :initial-contents '(#*010 #*101)))
         (ba3 (make-array '(2 3) :element-type 'bit))
         (ba4 (bit-not ba1 ba3)))
    (and (eq ba3 ba4)
         (equalp ba3
                 (make-array '(2 3)
                             :element-type 'bit
                             :initial-contents '(#*101 #*010)))
         (not (eq ba1 ba3))
         (not (eq ba1 ba4))))))
(define-test sacla-must-array.309 (:tag :sacla)
 (assert-true (bit-vector-p (make-array 6 :element-type 'bit :fill-pointer t))))
(define-test sacla-must-array.310 (:tag :sacla) (assert-true (bit-vector-p #*)))
(define-test sacla-must-array.311 (:tag :sacla)
 (assert-true (not (bit-vector-p (make-array 6)))))
(define-test sacla-must-array.312 (:tag :sacla)
 (assert-true (not (simple-bit-vector-p (make-array 6)))))
(define-test sacla-must-array.313 (:tag :sacla)
 (assert-true (simple-bit-vector-p #*)))
(define-test sacla-must-array.314 (:tag :sacla)
 (assert-true (simple-bit-vector-p #*0101)))
(define-test sacla-must-array.315 (:tag :sacla)
 (assert-true (simple-bit-vector-p #*0)))
(define-test sacla-must-array.316 (:tag :sacla)
 (assert-true (simple-bit-vector-p #*1)))
(define-test sacla-must-array.317 (:tag :sacla)
 (assert-true (simple-bit-vector-p (make-array 6 :element-type 'bit))))
(define-test sacla-must-array.318 (:tag :sacla)
 (assert-true
  (equal
   (concatenate 'list
                (adjust-array (make-array 5 :initial-contents '(0 1 2 3 4))
                              10
                              :initial-element -1))
   '(0 1 2 3 4 -1 -1 -1 -1 -1))))
(define-test sacla-must-array.319 (:tag :sacla)
 (assert-true
  (let* ((array0
          (make-array '(3 2)
                      :initial-contents '((e0-0 e0-1) (e1-0 e1-1)
                                          (e2-0 e2-1))))
         (array (adjust-array array0 '(4 3) :initial-element 0)))
    (and (eq (aref array 0 0) 'e0-0)
         (eq (aref array 0 1) 'e0-1)
         (eql (aref array 0 2) '0)
         (eq (aref array 1 0) 'e1-0)
         (eq (aref array 1 1) 'e1-1)
         (eql (aref array 1 2) 0)
         (eq (aref array 2 0) 'e2-0)
         (eq (aref array 2 1) 'e2-1)
         (eql (aref array 2 2) 0)))))
(define-test sacla-must-array.320 (:tag :sacla)
 (assert-true
  (let* ((array0
          (make-array '(3 2)
                      :initial-contents '((e0-0 e0-1) (e1-0 e1-1)
                                          (e2-0 e2-1))))
         (array (adjust-array array0 '(1 1) :initial-element 0)))
    (eq (aref array 0 0) 'e0-0))))
(define-test sacla-must-array.321 (:tag :sacla)
 (assert-true
  (let* ((array0 (make-array '(3 2) :initial-element 0))
         (array1 (make-array 6 :initial-element 1))
         (array (adjust-array array1 3 :displaced-to array0)))
    (and (equal (array-dimensions array) '(3)) (every #'zerop array)))))
(define-test sacla-must-array.322 (:tag :sacla)
 (assert-true
  (let* ((array0 (make-array '(3 2) :initial-contents '((0 1) (2 3) (4 5))))
         (array1 (make-array 6 :initial-element 1))
         (array
          (adjust-array array1
                        3
                        :displaced-to array0
                        :displaced-index-offset 3)))
    (and (equal (array-dimensions array) '(3))
         (eql (aref array 0) 3)
         (eql (aref array 1) 4)
         (eql (aref array 2) 5)))))
(define-test sacla-must-array.323 (:tag :sacla)
 (assert-true
  (let* ((array0 (make-array '(3 2) :initial-contents '((0 1) (2 3) (4 5))))
         (array1 (make-array 6 :displaced-to array0))
         (array (adjust-array array1 9 :initial-element '-1)))
    (and (equal (array-dimensions array) '(9))
         (multiple-value-bind (displaced-to displaced-index-offset)
             (array-displacement array)
           (and (null displaced-to) (zerop displaced-index-offset)))
         (eql (aref array 0) 0)
         (eql (aref array 1) 1)
         (eql (aref array 2) 2)
         (eql (aref array 3) 3)
         (eql (aref array 4) 4)
         (eql (aref array 5) 5)
         (eql (aref array 6) -1)
         (eql (aref array 7) -1)
         (eql (aref array 8) -1)))))
(define-test sacla-must-array.324 (:tag :sacla)
 (assert-true
  (let* ((array0
          (make-array '(4 4)
                      :adjustable t
                      :initial-contents '((alpha beta gamma delta)
                                          (epsilon zeta eta theta)
                                          (iota kappa lambda mu)
                                          (nu xi omicron pi))))
         (array (adjust-array array0 '(3 5) :initial-element 'baz)))
    (equalp array
            #2A((alpha beta gamma delta baz)
                (epsilon zeta eta theta baz)
                (iota kappa lambda mu baz))))))
(define-test sacla-must-array.325 (:tag :sacla)
 (assert-true
  (let* ((array0 (make-array 3 :initial-element 0))
         (array1 (make-array 3 :adjustable t :displaced-to array0))
         (array2 (make-array 3 :displaced-to array1)))
    (and (adjustable-array-p array1)
         (eq array1 (adjust-array array1 6 :initial-contents '(a b c d e f)))
         (multiple-value-bind (displaced-to displaced-index-offset)
             (array-displacement array1)
           (and (null displaced-to) (zerop displaced-index-offset)))
         (eq (aref array1 0) 'a)
         (eq (aref array1 1) 'b)
         (eq (aref array1 2) 'c)
         (eq (aref array1 3) 'd)
         (eq (aref array1 4) 'e)
         (eq (aref array1 5) 'f)
         (eq (aref array2 0) 'a)
         (eq (aref array2 1) 'b)
         (eq (aref array2 2) 'c)))))
(define-test sacla-must-array.326 (:tag :sacla)
 (assert-true
  (let* ((str0
          (make-array 10
                      :element-type 'character
                      :initial-contents "abcdefghij"))
         (str1
          (make-array 7
                      :adjustable t
                      :element-type 'character
                      :displaced-to str0
                      :displaced-index-offset 3))
         (str2
          (make-array 3
                      :element-type 'character
                      :displaced-to str1
                      :displaced-index-offset 4)))
    (and (string= str0 "abcdefghij")
         (string= str1 "defghij")
         (string= str2 "hij")
         (adjustable-array-p str1)
         (eq str1 (adjust-array str1 10 :initial-contents "QRSTUVWXYZ"))
         (string= str2 "UVW")))))
(define-test sacla-must-array.327 (:tag :sacla)
 (assert-true
  (let* ((bv
          (make-array 10
                      :element-type 'bit
                      :initial-contents #*1010101010
                      :fill-pointer t)))
    (and
     (dotimes (i 10 t)
       (unless (eql (vector-pop bv) (if (evenp i) 0 1))
         (return nil)))
     (zerop (length bv))))))
(define-test sacla-must-array.328 (:tag :sacla)
 (assert-true
  (let* ((bv (make-array 10 :adjustable t :element-type 'bit :fill-pointer 0)))
    (dotimes (i 100) (vector-push-extend (if (oddp i) 0 1) bv))
    (dotimes (i 100 t)
      (unless (eql (vector-pop bv) (if (oddp i) 1 0))
        (return nil))))))
(define-test sacla-must-array.329 (:tag :sacla)
 (assert-true
  (let* ((str
          (make-array 10
                      :element-type 'character
                      :initial-contents "abcdefghjk"
                      :fill-pointer t)))
    (and
     (dotimes (i 10 t)
       (unless (char= (vector-pop str) (aref "kjhgfedcba" i))
         (return nil)))
     (zerop (length str))))))
(define-test sacla-must-array.330 (:tag :sacla)
 (assert-true
  (let* ((str
          (make-array 10
                      :adjustable t
                      :element-type 'character
                      :fill-pointer 0)))
    (dotimes (i 100) (vector-push-extend (if (oddp i) #\a #\z) str))
    (dotimes (i 100 t)
      (unless (char= (vector-pop str) (if (oddp i) #\z #\a))
        (return nil))))))

