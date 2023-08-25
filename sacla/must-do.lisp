(in-package #:sacla-lisp-unit)
(define-test sacla-must-do.1 (:tag :sacla)
 (assert-true (null (dotimes (i 10)))))
(define-test sacla-must-do.2 (:tag :sacla)
 (assert-true (= (dotimes (temp-one 10 temp-one)) 10)))
(define-test sacla-must-do.3 (:tag :sacla)
 (assert-true
  (let ((temp-two 0))
    (and (eq t (dotimes (temp-one 10 t) (incf temp-two))) (eql temp-two 10)))))
(define-test sacla-must-do.4 (:tag :sacla)
 (assert-true
  (progn
    (defun palindromep (string &optional (start 0) (end (length string)))
      (dotimes (k (floor (- end start) 2) t)
        (unless (char-equal (char string (+ start k)) (char string (- end k 1)))
          (return nil))))
    (and (palindromep "Able was I ere I saw Elba")
         (not (palindromep "A man, a plan, a canal--Panama!"))
         (equal
          (remove-if-not #'alpha-char-p "A man, a plan, a canal--Panama!")
          "AmanaplanacanalPanama")
         (palindromep
          (remove-if-not #'alpha-char-p "A man, a plan, a canal--Panama!"))
         (palindromep
          (remove-if-not #'alpha-char-p
                         "Unremarkable was I ere I saw Elba Kramer, nu?"))))))
(define-test sacla-must-do.5 (:tag :sacla)
 (assert-true
  (let ((count 0))
    (eql (dotimes (i 5 count) (incf count)) 5))))
(define-test sacla-must-do.6 (:tag :sacla)
 (assert-true
  (let ((count 0))
    (eql (dotimes (i 1 count) (incf count)) 1))))
(define-test sacla-must-do.7 (:tag :sacla)
 (assert-true
  (let ((count 0))
    (zerop (dotimes (i 0 count) (incf count))))))
(define-test sacla-must-do.8 (:tag :sacla)
 (assert-true
  (let ((count 0))
    (zerop (dotimes (i -1 count) (incf count))))))
(define-test sacla-must-do.9 (:tag :sacla)
 (assert-true
  (let ((count 0))
    (zerop (dotimes (i -100 count) (incf count))))))
(define-test sacla-must-do.10 (:tag :sacla)
 (assert-true (eql (dotimes (i 3 i)) 3)))
(define-test sacla-must-do.11 (:tag :sacla)
 (assert-true (eql (dotimes (i 2 i)) 2)))
(define-test sacla-must-do.12 (:tag :sacla)
 (assert-true (eql (dotimes (i 1 i)) 1)))
(define-test sacla-must-do.13 (:tag :sacla)
 (assert-true (eql (dotimes (i 0 i)) 0)))
(define-test sacla-must-do.14 (:tag :sacla)
 (assert-true (eql (dotimes (i -1 i)) 0)))
(define-test sacla-must-do.15 (:tag :sacla)
 (assert-true (eql (dotimes (i -2 i)) 0)))
(define-test sacla-must-do.16 (:tag :sacla)
 (assert-true (eql (dotimes (i -10 i)) 0)))
(define-test sacla-must-do.17 (:tag :sacla)
 (assert-true
  (let ((list nil))
    (and (eq (dotimes (i 10 t) (push i list)) t)
         (equal list '(9 8 7 6 5 4 3 2 1 0))))))
(define-test sacla-must-do.18 (:tag :sacla)
 (assert-true
  (let ((list nil))
    (equal (dotimes (i 10 (push i list)) (push i list))
           '(10 9 8 7 6 5 4 3 2 1 0)))))
(define-test sacla-must-do.19 (:tag :sacla)
 (assert-true
  (let ((list nil))
    (equal (dotimes (i '10 (push i list)) (push i list))
           '(10 9 8 7 6 5 4 3 2 1 0)))))
(define-test sacla-must-do.20 (:tag :sacla)
 (assert-true
  (let ((list nil))
    (equal (dotimes (i (/ 100 10) (push i list)) (push i list))
           '(10 9 8 7 6 5 4 3 2 1 0)))))
(define-test sacla-must-do.21 (:tag :sacla)
 (assert-true (null (dotimes (i 10 t) (return nil)))))
(define-test sacla-must-do.22 (:tag :sacla)
 (assert-true
  (equal (multiple-value-list (dotimes (i 10 t) (return (values 'a 'b 'c))))
         '(a b c))))
(define-test sacla-must-do.23 (:tag :sacla)
 (assert-true
  (let ((val 0))
    (=
     (dotimes (i 10 val)
       (incf val 1)
       (when (< i 9)
         (go lp))
       (incf val 2)
      lp
       (incf val 3))
     42))))
(define-test sacla-must-do.24 (:tag :sacla)
 (assert-true
  (=
   (let ((val 0))
     (dotimes (i 10 val)
       (when (< i 9)
         (go loop))
      9
       (incf val 100)
       (go last)
      loop
       (when (= i 0)
         (go 9))
       (incf val)
      last))
   208)))
(define-test sacla-must-do.25 (:tag :sacla)
 (assert-true
  (= 3
     (let ((i 3))
       (dotimes (i i i) (declare (fixnum i)))))))
(define-test sacla-must-do.26 (:tag :sacla)
 (assert-true
  (= 3
     (let ((x 0))
       (dotimes (i 3 x) (declare (fixnum i)) (incf x))))))
(define-test sacla-must-do.27 (:tag :sacla)
 (assert-true (= 3 (dotimes (i 3 i) (declare (fixnum i))))))
(define-test sacla-must-do.28 (:tag :sacla)
 (assert-true
  (= 3
     (let ((x 0))
       (dotimes (i 3 x) (declare (fixnum i)) (incf x))))))
(define-test sacla-must-do.29 (:tag :sacla)
 (assert-true
  (equal '((8 6 4 2 0) (9 7 5 3 1))
         (let (even odd)
           (dotimes (i 10 (list even odd))
             (cond
               ((evenp i)
                (go even))
               ((oddp i)
                (go odd))
               (t
                (error "logic error")))
            even
             (push i even)
             (go end)
            odd
             (push i odd)
             (go end)
            end)))))
(define-test sacla-must-do.30 (:tag :sacla)
 (assert-true
  (let ((list (copy-tree '((0) (1) (2) (3)))))
    (and (null (dolist (item list) (incf (car item))))
         (equal list '((1) (2) (3) (4)))))))
(define-test sacla-must-do.31 (:tag :sacla)
 (assert-true (eq 'ok (dolist (x '(0 1 2) t) (return 'ok)))))
(define-test sacla-must-do.32 (:tag :sacla)
 (assert-true (eq 'ok (dolist (x '(0 1 2) t) (return-from nil 'ok)))))
(define-test sacla-must-do.33 (:tag :sacla)
 (assert-true
  (equal '(ok fine)
         (multiple-value-list
          (dolist (x '(0 1 2) t) (return (values 'ok 'fine)))))))
(define-test sacla-must-do.34 (:tag :sacla)
 (assert-true
  (equal '(ok fine)
         (multiple-value-list
          (dolist (x '(0 1 2) t) (return-from nil (values 'ok 'fine)))))))
(define-test sacla-must-do.35 (:tag :sacla)
 (assert-true
  (null
   (let ((x '(0 1 2)))
     (dolist (x x x))))))
(define-test sacla-must-do.36 (:tag :sacla)
 (assert-true
  (= 3
     (let ((x '(0 1 2)) (i 0))
       (dolist (x x i) (incf i))))))
(define-test sacla-must-do.37 (:tag :sacla)
 (assert-true (null (dolist (x 'nil)))))
(define-test sacla-must-do.38 (:tag :sacla)
 (assert-true (null (dolist (x '(a))))))
(define-test sacla-must-do.39 (:tag :sacla)
 (assert-true (eq t (dolist (x nil t)))))
(define-test sacla-must-do.40 (:tag :sacla)
 (assert-true
  (= 6
     (let ((sum 0))
       (dolist (x '(0 1 2 3) sum) (declare (fixnum x)) (incf sum x))))))
(define-test sacla-must-do.41 (:tag :sacla)
 (assert-true
  (equal '(5 4 3 2 1)
         (let (stack)
           (flet ((f ()
                    (declare (special x))
                    (1+ x)))
             (dolist (x '(0 1 2 3 4) stack)
               (declare (special x))
               (declare (type fixnum x))
               (push (f) stack)))))))
(define-test sacla-must-do.42 (:tag :sacla)
 (assert-true
  (equal '((3 1) (4 2 0))
         (let (odd even)
           (dolist (x '(0 1 2 3 4) (list odd even))
             (cond
               ((oddp x)
                (go odd))
               ((evenp x)
                (go even))
               (t
                (error "This code mustn't have got executed.")))
            odd
             (push x odd)
             (go loop-end)
            even
             (push x even)
             (go loop-end)
            loop-end)))))
(define-test sacla-must-do.43 (:tag :sacla)
 (assert-true
  (let ((temp-two 'nil))
    (equal (dolist (temp-one '(1 2 3 4) temp-two) (push temp-one temp-two))
           '(4 3 2 1)))))
(define-test sacla-must-do.44 (:tag :sacla)
 (assert-true
  (let ((temp-two 0))
    (and (null (dolist (temp-one '(1 2 3 4)) (incf temp-two)))
         (eql temp-two 4)))))
(define-test sacla-must-do.45 (:tag :sacla)
 (assert-true (null (dolist (var nil var)))))
(define-test sacla-must-do.46 (:tag :sacla)
 (assert-true
  (let ((list nil))
    (equal (dolist (var '(0 1 2 3) list) (push var list)) '(3 2 1 0)))))
(define-test sacla-must-do.47 (:tag :sacla)
 (assert-true
  (let ((list nil))
    (equal (dolist (var '(0 1 2 3) (push var list)) (push var list))
           '(nil 3 2 1 0)))))
(define-test sacla-must-do.48 (:tag :sacla)
 (assert-true (null (dolist (var '(0 1 2 3))))))
(define-test sacla-must-do.49 (:tag :sacla)
 (assert-true
  (let ((list nil))
    (and (null (dolist (var '(0 1 2 3)) (push var list)))
         (equal list '(3 2 1 0))))))
(define-test sacla-must-do.50 (:tag :sacla)
 (assert-true
  (let ((list nil))
    (and (eq (dolist (var 'nil t) (push var list)) t) (null list)))))
(define-test sacla-must-do.51 (:tag :sacla)
 (assert-true
  (let ((list '((a) (b) (c))) (count 0))
    (dolist (var list t)
      (unless (eq (nth count list) var)
        (return nil))
      (incf count)))))
(define-test sacla-must-do.52 (:tag :sacla)
 (assert-true
  (let ((list nil))
    (and
     (null (dolist (var '(0 1 2 3) t) (if (= var 2) (return) (push var list))))
     (equal list '(1 0))))))
(define-test sacla-must-do.53 (:tag :sacla)
 (assert-true
  (let ((val 0))
    (=
     (dolist (var '(a b c) val)
       (incf val 1)
       (unless (eq var 'c)
         (go lp))
       (incf val 2)
      lp
       (incf val 3))
     14))))
(define-test sacla-must-do.54 (:tag :sacla)
 (assert-true
  (=
   (let ((val 0))
     (dolist (i '(0 1 2 3 4 5 6 7 8 9) val)
       (when (< i 9)
         (go loop))
      9
       (incf val 100)
       (go last)
      loop
       (when (= i 0)
         (go 9))
       (incf val)
      last))
   208)))
(define-test sacla-must-do.55 (:tag :sacla)
 (assert-true
  (let ((val 0))
    (=
     (dolist (i '(0 1 2 3 4 5 6 7 8 9) val)
       (incf val 1)
       (when (< i 9)
         (go lp))
       (incf val 2)
      lp
       (incf val 3))
     42))))
(define-test sacla-must-do.56 (:tag :sacla)
 (assert-true
  (eq 'ok
      (block nil
        (tagbody
          (dolist (x '(0 1 2 3) t)
            (when (oddp x)
              (go there)))
         there
          (return 'ok))))))
(define-test sacla-must-do.57 (:tag :sacla)
 (assert-true
  (flet ((rev (list)
           (do ((x list (cdr x))
                (reverse nil (cons (car x) reverse)))
               ((null x) reverse))))
    (and (null (rev nil)) (equal (rev '(0 1 2 3 4)) '(4 3 2 1 0))))))
(define-test sacla-must-do.58 (:tag :sacla)
 (assert-true
  (flet ((nrev (list)
           (do ((1st (cdr list) (cdr 1st))
                (2nd list 1st)
                (3rd 'nil 2nd))
               ((null 2nd) 3rd)
             (rplacd 2nd 3rd))))
    (and (null (nrev nil)) (equal (nrev (list 0 1 2 3 4)) '(4 3 2 1 0))))))
(define-test sacla-must-do.59 (:tag :sacla)
 (assert-true
  (flet ((sub (list start end)
           (do* ((x (nthcdr start list) (cdr x))
                 (i start (1+ i))
                 (result (list nil))
                 (splice result))
                ((>= i end) (cdr result))
             (setq splice (cdr (rplacd splice (list (car x))))))))
    (and (eq (sub 'nil 0 0) 'nil)
         (equal (sub '(0 1 2 3) 1 4) '(1 2 3))
         (equal (sub '(0 1 2 3) 1 1) 'nil)
         (equal (sub '(0 1 2 3) 1 2) '(1))
         (equal (sub '(0 1 2 3) 1 3) '(1 2))))))
(define-test sacla-must-do.60 (:tag :sacla)
 (assert-true
  (eql
   (do ((temp-one 1 (1+ temp-one))
        (temp-two 0 (1- temp-two)))
       ((> (- temp-one temp-two) 5) temp-one))
   4)))
(define-test sacla-must-do.61 (:tag :sacla)
 (assert-true
  (eql
   (do ((temp-one 1 (1+ temp-one))
        (temp-two 0 (1+ temp-one)))
       ((= 3 temp-two) temp-one))
   3)))
(define-test sacla-must-do.62 (:tag :sacla)
 (assert-true
  (eql
   (do* ((temp-one 1 (1+ temp-one))
         (temp-two 0 (1+ temp-one)))
        ((= 3 temp-two) temp-one))
   2)))
(define-test sacla-must-do.63 (:tag :sacla)
 (assert-true
  (let ((a-vector (vector 1 nil 3 nil)))
    (and
     (null
      (do ((i 0 (+ i 1))
           (n (array-dimension a-vector 0)))
          ((= i n))
        (when (null (aref a-vector i))
          (setf (aref a-vector i) 0))))
     (equalp a-vector #(1 0 3 0))))))
(define-test sacla-must-do.64 (:tag :sacla)
 (assert-true
  (let ((vec (vector 0 1 2 3 4 5 6 7 8 9)))
    (equalp
     (do ((i 0 (1+ i))
          n
          (j 9 (1- j)))
         ((>= i j) vec)
       (setq n (aref vec i))
       (setf (aref vec i) (aref vec j))
       (setf (aref vec j) n))
     #(9 8 7 6 5 4 3 2 1 0)))))
(define-test sacla-must-do.65 (:tag :sacla)
 (assert-true
  (let ((vec (vector 0 1 2 3 4 5 6 7 8 9)))
    (and
     (null
      (do ((i 0 (1+ i))
           n
           (j 9 (1- j)))
          ((>= i j))
        (setq n (aref vec i))
        (setf (aref vec i) (aref vec j))
        (setf (aref vec j) n)))
     (equalp vec #(9 8 7 6 5 4 3 2 1 0))))))
(define-test sacla-must-do.66 (:tag :sacla)
 (assert-true
  (let ((vec (vector 0 1 2 3 4 5 6 7 8 9)))
    (and
     (null
      (do ((i 0 (1+ i))
           n
           (j 9 (1- j)))
          ((>= i j))
        (declare (fixnum i j n))
        (setq n (aref vec i))
        (setf (aref vec i) (aref vec j))
        (setf (aref vec j) n)))
     (equalp vec #(9 8 7 6 5 4 3 2 1 0))))))
(define-test sacla-must-do.67 (:tag :sacla)
 (assert-true
  (let ((vec (vector 0 1 2 3 4 5 6 7 8 9)))
    (and
     (null
      (do ((i 0 (1+ i))
           n
           (j 9 (1- j)))
          ((>= i j))
        (declare (fixnum i))
        (declare (fixnum j))
        (declare (fixnum n))
        (setq n (aref vec i))
        (setf (aref vec i) (aref vec j))
        (setf (aref vec j) n)))
     (equalp vec #(9 8 7 6 5 4 3 2 1 0))))))
(define-test sacla-must-do.68 (:tag :sacla)
 (assert-true
  (let ((vec (vector 0 1 2 3 4 5 6 7 8 9)))
    (and
     (null
      (do (n
           (i 0 (1+ i))
           (j 9 (1- j)))
          ((>= i j))
        (declare (fixnum i))
        (declare (fixnum j))
        (declare (fixnum n))
        (setq n (aref vec i))
        (setf (aref vec i) (aref vec j))
        (setf (aref vec j) n)))
     (equalp vec #(9 8 7 6 5 4 3 2 1 0))))))
(define-test sacla-must-do.69 (:tag :sacla)
 (assert-true
  (let ((vec (vector 0 1 2 3 4 5 6 7 8 9)))
    (and
     (null
      (do ((i 0 (1+ i))
           (j 9 (1- j))
           n)
          ((>= i j))
        (declare (fixnum i))
        (declare (fixnum j))
        (declare (fixnum n))
        (setq n (aref vec i))
        (setf (aref vec i) (aref vec j))
        (setf (aref vec j) n)))
     (equalp vec #(9 8 7 6 5 4 3 2 1 0))))))
(define-test sacla-must-do.70 (:tag :sacla)
 (assert-true
  (=
   (do* ((list (list 0 1 2 3 4 5 6 7 8 9) (cdr list))
         (elm (car list) (car list))
         (n 0 (+ n (or elm 0))))
        ((endp list) n))
   45)))
(define-test sacla-must-do.71 (:tag :sacla)
 (assert-true
  (=
   (do* ((list (list 0 1 2 3 4 5 6 7 8 9) (cdr list))
         (elm (car list) (car list))
         (n 0))
        ((endp list) n)
     (incf n elm))
   45)))
(define-test sacla-must-do.72 (:tag :sacla)
 (assert-true
  (let ((vec (vector 0 1 2 3 4 5 6 7 8 9)))
    (and
     (null
      (do* (n
            (i 0 (1+ i))
            (j (- 9 i) (- 9 i)))
           ((>= i j))
        (declare (fixnum i))
        (declare (fixnum j))
        (declare (fixnum n))
        (setq n (aref vec i))
        (setf (aref vec i) (aref vec j))
        (setf (aref vec j) n)))
     (equalp vec #(9 8 7 6 5 4 3 2 1 0))))))
(define-test sacla-must-do.73 (:tag :sacla)
 (assert-true
  (let ((vec (vector 0 1 2 3 4 5 6 7 8 9)))
    (and
     (null
      (do* ((i 0 (1+ i))
            n
            (j (- 9 i) (- 9 i)))
           ((>= i j))
        (declare (fixnum i j n))
        (setq n (aref vec i))
        (setf (aref vec i) (aref vec j))
        (setf (aref vec j) n)))
     (equalp vec #(9 8 7 6 5 4 3 2 1 0))))))
(define-test sacla-must-do.74 (:tag :sacla)
 (assert-true
  (let ((vec (vector 0 1 2 3 4 5 6 7 8 9)))
    (and
     (null
      (do* ((i 0 (1+ i))
            (j (- 9 i) (- 9 i))
            n)
           ((>= i j))
        (declare (fixnum i j n))
        (setq n (aref vec i))
        (setf (aref vec i) (aref vec j))
        (setf (aref vec j) n)))
     (equalp vec #(9 8 7 6 5 4 3 2 1 0))))))
(define-test sacla-must-do.75 (:tag :sacla)
 (assert-true
  (let ((vec (vector 0 1 2 3 4 5 6 7 8 9)))
    (and
     (null
      (do* ((i 0 (1+ i))
            (j (- 9 i) (- 9 i))
            n)
           ((>= i j))
        (setf n (aref vec i) (aref vec i) (aref vec j) (aref vec j) n)))
     (equalp vec #(9 8 7 6 5 4 3 2 1 0))))))

