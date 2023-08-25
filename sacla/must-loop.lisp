(in-package #:sacla-lisp-unit)
(define-test sacla-must-loop.1 (:tag :sacla)
 (assert-true
  (null
   (loop
      (return)))))
(define-test sacla-must-loop.2 (:tag :sacla)
 (assert-true
  (loop
     (return-from nil t))))
(define-test sacla-must-loop.3 (:tag :sacla)
 (assert-true
  (null
   (let ((stack '(0 1 2)))
     (loop
        (unless (pop stack)
          (return)))
     stack))))
(define-test sacla-must-loop.4 (:tag :sacla)
 (assert-true
  (equal
   (multiple-value-list
    (loop
       (return (values 0 1 2))))
   '(0 1 2))))
(define-test sacla-must-loop.5 (:tag :sacla)
 (assert-true
  (= 100
     (let ((i 0))
       (loop
          (incf i)
          (when (>= i 100)
            (return i)))))))
(define-test sacla-must-loop.6 (:tag :sacla)
 (assert-true
  (eq
   (let (x)
     (tagbody
       (loop
          (go end))
      end
       (setq x t))
     x)
   t)))
(define-test sacla-must-loop.7 (:tag :sacla)
 (assert-true
  (eq t
      (catch 'end
        (loop
           (throw 'end t))))))
(define-test sacla-must-loop.8 (:tag :sacla)
 (assert-true
  (eq t
      (block here
        (loop
           (return-from here t))))))
(define-test sacla-must-loop.9 (:tag :sacla)
 (assert-true
  (= 3
     (let ((i 0))
       (loop
          (incf i)
          (if (= i 3) (return i)))))))
(define-test sacla-must-loop.10 (:tag :sacla)
 (assert-true
  (= 9
     (let ((i 0) (j 0))
       (tagbody
         (loop
            (incf j 3)
            (incf i)
            (if (= i 3) (go exit)))
        exit)
       j))))
(define-test sacla-must-loop.11 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop :for a :from 1 :to 3 :by 1 :do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.12 (:tag :sacla)
 (assert-true
  (let ((for (make-symbol "FOR"))
        (from (make-symbol "FROM"))
        (to (make-symbol "TO"))
        (by (make-symbol "BY"))
        (do (make-symbol "DO")))
    (equal
     (eval
      `(let (stack)
         (loop
            ,for
            a
            ,from
            ,1
            ,to
            ,3
            ,by
            ,1
            ,do
            (push a stack))
         stack))
     '(3 2 1)))))
(define-test sacla-must-loop.13 (:tag :sacla)
 (assert-true
  (let ((for (make-symbol "FOR")))
    (equal
     (eval
      `(let (stack)
         (loop
            ,for
            a
            :from
            1
            :to
            3
            :by
            1
            :do
            (push a stack))
         stack))
     '(3 2 1)))))
(define-test sacla-must-loop.14 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "LOOP-KEY-TEST")
      (delete-package "LOOP-KEY-TEST"))
    (let* ((pkg (defpackage "LOOP-KEY-TEST"))
           (for (intern "FOR" pkg))
           (in (intern "IN" pkg))
           (by
            (progn
              (import 'by pkg)
              (intern "BY" pkg)))
           (collect
            (progn
              (import 'collect pkg)
              (intern "COLLECT" pkg))))
      (export collect pkg)
      (and
       (equal
        (eval
         `(loop
             ,for
             elt
             ,in
             '(1 2 3 4 5)
             ,by
             #'cddr
             ,collect
             elt))
        '(1 3 5))
       (delete-package pkg))))))
(define-test sacla-must-loop.15 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a from 1 to 3 by 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.16 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a from 1 by 1 to 3 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.17 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a to 3 by 1 from 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.18 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a to 3 from 1 by 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.19 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a by 1 to 3 from 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.20 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a by 1 from 1 to 3 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.21 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a upfrom 1 to 3 by 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.22 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a upfrom 1 by 1 to 3 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.23 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a to 3 by 1 upfrom 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.24 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a to 3 upfrom 1 by 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.25 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a by 1 to 3 upfrom 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.26 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a by 1 upfrom 1 to 3 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.27 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a from 1 upto 3 by 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.28 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a from 1 by 1 upto 3 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.29 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a upto 3 by 1 from 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.30 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a upto 3 from 1 by 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.31 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a by 1 upto 3 from 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.32 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a by 1 from 1 upto 3 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.33 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a upfrom 1 upto 3 by 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.34 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a upfrom 1 by 1 upto 3 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.35 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a upto 3 by 1 upfrom 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.36 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a upto 3 upfrom 1 by 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.37 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a by 1 upto 3 upfrom 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.38 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a by 1 upfrom 1 upto 3 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.39 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a from 1 below 4 by 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.40 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a from 1 by 1 below 4 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.41 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a below 4 by 1 from 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.42 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a below 4 from 1 by 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.43 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a by 1 below 4 from 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.44 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a by 1 from 1 below 4 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.45 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a upfrom 1 below 4 by 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.46 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a upfrom 1 by 1 below 4 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.47 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a below 4 by 1 upfrom 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.48 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a below 4 upfrom 1 by 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.49 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a by 1 below 4 upfrom 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.50 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a by 1 upfrom 1 below 4 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.51 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a from 1 to 3 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.52 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a to 3 from 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.53 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a upfrom 1 to 3 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.54 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a to 3 upfrom 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.55 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a from 1 upto 3 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.56 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a upto 3 from 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.57 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a upfrom 1 upto 3 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.58 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a upto 3 upfrom 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.59 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a from 1 below 4 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.60 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a below 4 from 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.61 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a upfrom 1 below 4 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.62 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a below 4 upfrom 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.63 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a to 3 by 1 do (push a stack))
     stack)
   '(3 2 1 0))))
(define-test sacla-must-loop.64 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a by 1 to 3 do (push a stack))
     stack)
   '(3 2 1 0))))
(define-test sacla-must-loop.65 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a upto 3 by 1 do (push a stack))
     stack)
   '(3 2 1 0))))
(define-test sacla-must-loop.66 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a by 1 upto 3 do (push a stack))
     stack)
   '(3 2 1 0))))
(define-test sacla-must-loop.67 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a below 4 by 1 do (push a stack))
     stack)
   '(3 2 1 0))))
(define-test sacla-must-loop.68 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a by 1 below 4 do (push a stack))
     stack)
   '(3 2 1 0))))
(define-test sacla-must-loop.69 (:tag :sacla)
 (assert-true
  (= 4
     (let ((stack '(1 2 3)))
       (loop for a from 1 by 1
             do (unless (pop stack)
                  (return a)))))))
(define-test sacla-must-loop.70 (:tag :sacla)
 (assert-true
  (= 4
     (let ((stack '(1 2 3)))
       (loop for a by 1 from 1
             do (unless (pop stack)
                  (return a)))))))
(define-test sacla-must-loop.71 (:tag :sacla)
 (assert-true
  (= 4
     (let ((stack '(1 2 3)))
       (loop for a upfrom 1 by 1
             do (unless (pop stack)
                  (return a)))))))
(define-test sacla-must-loop.72 (:tag :sacla)
 (assert-true
  (= 4
     (let ((stack '(1 2 3)))
       (loop for a by 1 upfrom 1
             do (unless (pop stack)
                  (return a)))))))
(define-test sacla-must-loop.73 (:tag :sacla)
 (assert-true
  (= 4
     (let ((stack '(1 2 3)))
       (loop for a from 1
             do (unless (pop stack)
                  (return a)))))))
(define-test sacla-must-loop.74 (:tag :sacla)
 (assert-true
  (= 4
     (let ((stack '(1 2 3)))
       (loop for a upfrom 1
             do (unless (pop stack)
                  (return a)))))))
(define-test sacla-must-loop.75 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a to 3 do (push a stack))
     stack)
   '(3 2 1 0))))
(define-test sacla-must-loop.76 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a upto 3 do (push a stack))
     stack)
   '(3 2 1 0))))
(define-test sacla-must-loop.77 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a below 4 do (push a stack))
     stack)
   '(3 2 1 0))))
(define-test sacla-must-loop.78 (:tag :sacla)
 (assert-true
  (= 3
     (let ((stack '(1 2 3)))
       (loop for a by 1
             do (unless (pop stack)
                  (return a)))))))
(define-test sacla-must-loop.79 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a from 3 downto 1 by 1 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.80 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a from 3 by 1 downto 1 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.81 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a downto 1 by 1 from 3 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.82 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a downto 1 from 3 by 1 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.83 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a by 1 from 3 downto 1 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.84 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a by 1 downto 1 from 3 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.85 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a from 3 above 0 by 1 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.86 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a from 3 by 1 above 0 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.87 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a above 0 by 1 from 3 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.88 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a above 0 from 3 by 1 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.89 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a by 1 from 3 above 0 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.90 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a by 1 above 0 from 3 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.91 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a from 3 downto 1 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.92 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a downto 1 from 3 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.93 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a from 3 above 0 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.94 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a above 0 from 3 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.95 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a downfrom 3 to 1 by 1 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.96 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a downfrom 3 by 1 to 1 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.97 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a to 1 by 1 downfrom 3 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.98 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a to 1 downfrom 3 by 1 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.99 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a by 1 to 1 downfrom 3 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.100 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a by 1 downfrom 3 to 1 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.101 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a downfrom 3 downto 1 by 1 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.102 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a downfrom 3 by 1 downto 1 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.103 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a downto 1 by 1 downfrom 3 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.104 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a downto 1 downfrom 3 by 1 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.105 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a by 1 downto 1 downfrom 3 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.106 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a by 1 downfrom 3 downto 1 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.107 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a downfrom 3 above 0 by 1 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.108 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a downfrom 3 by 1 above 0 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.109 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a above 0 by 1 downfrom 3 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.110 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a above 0 downfrom 3 by 1 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.111 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a by 1 above 0 downfrom 3 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.112 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a by 1 downfrom 3 above 0 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.113 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a downfrom 3 to 1 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.114 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a to 1 downfrom 3 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.115 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a downfrom 3 downto 1 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.116 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a downto 1 downfrom 3 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.117 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a downfrom 3 above 0 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.118 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a above 0 downfrom 3 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.119 (:tag :sacla)
 (assert-true
  (zerop
   (let ((stack '(0 1 2)))
     (loop for a downfrom 3 by 1
           do (unless (pop stack)
                (return a)))))))
(define-test sacla-must-loop.120 (:tag :sacla)
 (assert-true
  (zerop
   (let ((stack '(0 1 2)))
     (loop for a by 1 downfrom 3
           do (unless (pop stack)
                (return a)))))))
(define-test sacla-must-loop.121 (:tag :sacla)
 (assert-true
  (zerop
   (let ((stack '(0 1 2)))
     (loop for a downfrom 3
           do (unless (pop stack)
                (return a)))))))
(define-test sacla-must-loop.122 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a from (+ 1 1) upto (+ 4 6) by (1+ 1) do (push a stack))
     stack)
   '(10 8 6 4 2))))
(define-test sacla-must-loop.123 (:tag :sacla)
 (assert-true
  (equal
   (let ((x 0) stack)
     (loop for a from (incf x) upto (+ (incf x) 10) by x do (push a stack))
     stack)
   '(11 9 7 5 3 1))))
(define-test sacla-must-loop.124 (:tag :sacla)
 (assert-true
  (equal
   (let ((x 0) stack)
     (loop for a from (incf x) by (incf x) upto (+ x 10) do (push a stack))
     stack)
   '(11 9 7 5 3 1))))
(define-test sacla-must-loop.125 (:tag :sacla)
 (assert-true
  (equal
   (let ((x 0) stack)
     (loop for a by (incf x) from (incf x) upto (+ x 10) do (push a stack))
     stack)
   '(12 11 10 9 8 7 6 5 4 3 2))))
(define-test sacla-must-loop.126 (:tag :sacla)
 (assert-true
  (equal
   (let ((x 0) stack)
     (loop for a by (incf x) upto (+ (incf x) 10) from (incf x)
           do (push a stack))
     stack)
   '(12 11 10 9 8 7 6 5 4 3))))
(define-test sacla-must-loop.127 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a t from 1 to 3 by 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.128 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a fixnum from 1 to 3 by 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.129 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a float from 1.0 to 3.0 by 1.0 do (push a stack))
     stack)
   '(3.0 2.0 1.0))))
(define-test sacla-must-loop.130 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a of-type t from 1 to 3 by 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.131 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a of-type fixnum from 1 to 3 by 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.132 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a of-type float from 1.0 to 3.0 by 1.0 do (push a stack))
     stack)
   '(3.0 2.0 1.0))))
(define-test sacla-must-loop.133 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a of-type number from 1 to 3 by 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.134 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a of-type integer from 1 to 3 by 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.135 (:tag :sacla)
 (assert-true
  (equal
   (let ((stack))
     (loop for a from 0 upto 10 by 5 do (push a stack))
     stack)
   '(10 5 0))))
(define-test sacla-must-loop.136 (:tag :sacla)
 (assert-true
  (equal
   (let ((stack))
     (loop for a from 0 upto 10 by 3 do (push a stack))
     stack)
   '(9 6 3 0))))
(define-test sacla-must-loop.137 (:tag :sacla)
 (assert-true
  (equal
   (let ((stack))
     (loop for a from -3 upto 0 do (push a stack))
     stack)
   '(0 -1 -2 -3))))
(define-test sacla-must-loop.138 (:tag :sacla)
 (assert-true
  (equal
   (let ((stack))
     (loop for a downfrom 0 to -3 do (push a stack))
     stack)
   '(-3 -2 -1 0))))
(define-test sacla-must-loop.139 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop as a from 1 to 3 by 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.140 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop as a upfrom 1 to 3 by 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.141 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop as a from 1 upto 3 by 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.142 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop as a upfrom 1 upto 3 by 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.143 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop as a from 1 below 4 by 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.144 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop as a upfrom 1 below 4 by 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.145 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop as a from 1 to 3 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.146 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop as a upfrom 1 to 3 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.147 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop as a from 1 upto 3 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.148 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop as a upfrom 1 upto 3 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.149 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop as a from 1 below 4 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.150 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop as a upfrom 1 below 4 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.151 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop as a to 3 by 1 do (push a stack))
     stack)
   '(3 2 1 0))))
(define-test sacla-must-loop.152 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop as a upto 3 by 1 do (push a stack))
     stack)
   '(3 2 1 0))))
(define-test sacla-must-loop.153 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop as a below 4 by 1 do (push a stack))
     stack)
   '(3 2 1 0))))
(define-test sacla-must-loop.154 (:tag :sacla)
 (assert-true
  (= 4
     (let ((stack '(1 2 3)))
       (loop as a from 1 by 1
             do (unless (pop stack)
                  (return a)))))))
(define-test sacla-must-loop.155 (:tag :sacla)
 (assert-true
  (= 4
     (let ((stack '(1 2 3)))
       (loop as a upfrom 1 by 1
             do (unless (pop stack)
                  (return a)))))))
(define-test sacla-must-loop.156 (:tag :sacla)
 (assert-true
  (= 4
     (let ((stack '(1 2 3)))
       (loop as a from 1
             do (unless (pop stack)
                  (return a)))))))
(define-test sacla-must-loop.157 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop as a to 3 do (push a stack))
     stack)
   '(3 2 1 0))))
(define-test sacla-must-loop.158 (:tag :sacla)
 (assert-true
  (= 3
     (let ((stack '(1 2 3)))
       (loop as a by 1
             do (unless (pop stack)
                  (return a)))))))
(define-test sacla-must-loop.159 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop as a from 3 downto 1 by 1 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.160 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop as a from 3 above 0 by 1 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.161 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop as a from 3 downto 1 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.162 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop as a from 3 above 0 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.163 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop as a downfrom 3 to 1 by 1 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.164 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop as a to 1 by 1 downfrom 3 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.165 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop as a by 1 to 1 downfrom 3 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.166 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop as a downfrom 3 downto 1 by 1 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.167 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop as a downto 1 by 1 downfrom 3 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.168 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop as a by 1 downto 1 downfrom 3 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.169 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop as a downfrom 3 above 0 by 1 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.170 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop as a above 0 by 1 downfrom 3 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.171 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop as a by 1 above 0 downfrom 3 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.172 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop as a downfrom 3 to 1 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.173 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop as a downfrom 3 downto 1 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.174 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop as a downfrom 3 above 0 do (push a stack))
     stack)
   '(1 2 3))))
(define-test sacla-must-loop.175 (:tag :sacla)
 (assert-true
  (zerop
   (let ((stack '(0 1 2)))
     (loop as a downfrom 3 by 1
           do (unless (pop stack)
                (return a)))))))
(define-test sacla-must-loop.176 (:tag :sacla)
 (assert-true
  (zerop
   (let ((stack '(0 1 2)))
     (loop as a downfrom 3
           do (unless (pop stack)
                (return a)))))))
(define-test sacla-must-loop.177 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a from 0 upto 0 do (push a stack))
     stack)
   '(0))))
(define-test sacla-must-loop.178 (:tag :sacla)
 (assert-true (null (loop for a upfrom 0 below 0))))
(define-test sacla-must-loop.179 (:tag :sacla)
 (assert-true (null (loop for a upfrom 10 to -10 collect a))))
(define-test sacla-must-loop.180 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a from 1/3 upto 1 by 1/3 do (push a stack))
     stack)
   '(1 2/3 1/3))))
(define-test sacla-must-loop.181 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a of-type rational from 1/3 upto 5/3 by 1/3 do (push a stack))
     stack)
   '(5/3 4/3 1 2/3 1/3))))
(define-test sacla-must-loop.182 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a fixnum below 3 do (push a stack))
     stack)
   '(2 1 0))))
(define-test sacla-must-loop.183 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a of-type fixnum below 3 do (push a stack))
     stack)
   '(2 1 0))))
(define-test sacla-must-loop.184 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a of-type (integer 0 2) below 3 do (push a stack))
     stack)
   '(2 1 0))))
(define-test sacla-must-loop.185 (:tag :sacla)
 (assert-true (null (loop for a in 'nil))))
(define-test sacla-must-loop.186 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a in '(0 1 2) do (push a stack))
     stack)
   '(2 1 0))))
(define-test sacla-must-loop.187 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a in
               (let ((i 0))
                 (list (incf i) (incf i) (incf i)))
           do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.188 (:tag :sacla)
 (assert-true
  (handler-case (loop for a in '(0 1 . 2))
    (type-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-loop.189 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a in '(0 1 2 3) by #'cdr do (push a stack))
     stack)
   '(3 2 1 0))))
(define-test sacla-must-loop.190 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a in '(0 1 2 3) by #'cddr do (push a stack))
     stack)
   '(2 0))))
(define-test sacla-must-loop.191 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a in '(0 1 2 3) by #'cdddr do (push a stack))
     stack)
   '(3 0))))
(define-test sacla-must-loop.192 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a in '(0 1 2 3) by #'cddddr do (push a stack))
     stack)
   '(0))))
(define-test sacla-must-loop.193 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a t in '(0 1 2) do (push a stack))
     stack)
   '(2 1 0))))
(define-test sacla-must-loop.194 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a of-type t in '(0 1 2) do (push a stack))
     stack)
   '(2 1 0))))
(define-test sacla-must-loop.195 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a fixnum in '(0 1 2) do (push a stack))
     stack)
   '(2 1 0))))
(define-test sacla-must-loop.196 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a of-type fixnum in '(0 1 2) do (push a stack))
     stack)
   '(2 1 0))))
(define-test sacla-must-loop.197 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a of-type t in '(0 1 2) do (push a stack))
     stack)
   '(2 1 0))))
(define-test sacla-must-loop.198 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a float in '(0.0 1.0 2.0) do (push a stack))
     stack)
   '(2.0 1.0 0.0))))
(define-test sacla-must-loop.199 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a of-type float in '(0.0 1.0 2.0) do (push a stack))
     stack)
   '(2.0 1.0 0.0))))
(define-test sacla-must-loop.200 (:tag :sacla)
 (assert-true (null (loop for a on 'nil))))
(define-test sacla-must-loop.201 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a on '(0 1 2) do (push a stack))
     stack)
   '((2) (1 2) (0 1 2)))))
(define-test sacla-must-loop.202 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a on
               (let ((i 0))
                 (list (incf i) (incf i) (incf i)))
           do (push (car a) stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.203 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a on '(0 1 . 2) do (push a stack))
     stack)
   '((1 . 2) (0 1 . 2)))))
(define-test sacla-must-loop.204 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a on '(0 1 2 3) by #'cdr do (push a stack))
     stack)
   '((3) (2 3) (1 2 3) (0 1 2 3)))))
(define-test sacla-must-loop.205 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a on '(0 1 2 3) by #'cddr do (push a stack))
     stack)
   '((2 3) (0 1 2 3)))))
(define-test sacla-must-loop.206 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a on '(0 1 2 3) by #'cdddr do (push a stack))
     stack)
   '((3) (0 1 2 3)))))
(define-test sacla-must-loop.207 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a on '(0 1 2 3) by #'cddddr do (push a stack))
     stack)
   '((0 1 2 3)))))
(define-test sacla-must-loop.208 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a t on '(0 1 2) do (push a stack))
     stack)
   '((2) (1 2) (0 1 2)))))
(define-test sacla-must-loop.209 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a of-type t on '(0 1 2) do (push a stack))
     stack)
   '((2) (1 2) (0 1 2)))))
(define-test sacla-must-loop.210 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a of-type list on '(0 1 2) do (push a stack))
     stack)
   '((2) (1 2) (0 1 2)))))
(define-test sacla-must-loop.211 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a on '(0 1 2 3) by #'(lambda (arg) (cddddr arg))
           do (push a stack))
     stack)
   '((0 1 2 3)))))
(define-test sacla-must-loop.212 (:tag :sacla)
 (assert-true (null (loop for a across ""))))
(define-test sacla-must-loop.213 (:tag :sacla)
 (assert-true
  (null
   (let (stack)
     (loop for a across "" do (push a stack))
     stack))))
(define-test sacla-must-loop.214 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a across "abc" do (push a stack))
     stack)
   '(#\c #\b #\a))))
(define-test sacla-must-loop.215 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a across #(x y z) do (push a stack))
     stack)
   '(z y x))))
(define-test sacla-must-loop.216 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a across #*0101 do (push a stack))
     stack)
   '(1 0 1 0))))
(define-test sacla-must-loop.217 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a t across "abc" do (push a stack))
     stack)
   '(#\c #\b #\a))))
(define-test sacla-must-loop.218 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a of-type t across "abc" do (push a stack))
     stack)
   '(#\c #\b #\a))))
(define-test sacla-must-loop.219 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a of-type character across "abc" do (push a stack))
     stack)
   '(#\c #\b #\a))))
(define-test sacla-must-loop.220 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a of-type base-char across "abc" do (push a stack))
     stack)
   '(#\c #\b #\a))))
(define-test sacla-must-loop.221 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a float across #(0.0 1.0 2.0) do (push a stack))
     stack)
   '(2.0 1.0 0.0))))
(define-test sacla-must-loop.222 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a of-type float across #(0.0 1.0 2.0) do (push a stack))
     stack)
   '(2.0 1.0 0.0))))
(define-test sacla-must-loop.223 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a fixnum across #(0 1 2) do (push a stack))
     stack)
   '(2 1 0))))
(define-test sacla-must-loop.224 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a of-type fixnum across #(0 1 2) do (push a stack))
     stack)
   '(2 1 0))))
(define-test sacla-must-loop.225 (:tag :sacla)
 (assert-true
  (=
   (let ((i 3))
     (loop for a = 0 then (1+ a)
           do (when (zerop (decf i))
                (return a))))
   2)))
(define-test sacla-must-loop.226 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a = '(0 1 2) then (cdr a)
           do (if a (push (car a) stack) (return stack))))
   '(2 1 0))))
(define-test sacla-must-loop.227 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop with i = 0
           for x = i
           do (when (= i 3)
                (return))
              (push x stack)
              (incf i))
     stack)
   '(2 1 0))))
(define-test sacla-must-loop.228 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for i = 0 then (1+ i) do (push i stack) when (= i 3) return t)
     stack)
   '(3 2 1 0))))
(define-test sacla-must-loop.229 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for i fixnum = 0 then (1+ i)
           do (push i stack)
           when (= i 3) return t)
     stack)
   '(3 2 1 0))))
(define-test sacla-must-loop.230 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for i of-type fixnum = 0 then (1+ i)
           do (push i stack)
           when (= i 3) return t)
     stack)
   '(3 2 1 0))))
(define-test sacla-must-loop.231 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for i float = 0.0 then (1+ i)
           do (push i stack)
           when (= i 3.0) return t)
     stack)
   '(3.0 2.0 1.0 0.0))))
(define-test sacla-must-loop.232 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for i of-type float = 0.0 then (1+ i)
           do (push i stack)
           when (= i 3.0) return t)
     stack)
   '(3.0 2.0 1.0 0.0))))
(define-test sacla-must-loop.233 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for i t = 0.0 then (1+ i) do (push i stack) when (= i 3.0) return t)
     stack)
   '(3.0 2.0 1.0 0.0))))
(define-test sacla-must-loop.234 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for i of-type t = 0.0 then (1+ i)
           do (push i stack)
           when (= i 3.0) return t)
     stack)
   '(3.0 2.0 1.0 0.0))))
(define-test sacla-must-loop.235 (:tag :sacla)
 (assert-true
  (let ((chars '(#\a #\b #\c #\d)))
    (eq t (loop for c = (pop chars) unless chars return t)))))
(define-test sacla-must-loop.236 (:tag :sacla)
 (assert-true
  (let ((chars '(#\a #\b #\c #\d)))
    (eq t (loop for c of-type character = (pop chars) unless chars return t)))))
(define-test sacla-must-loop.237 (:tag :sacla)
 (assert-true
  (let ((chars '(#\a #\b #\c #\d)))
    (eq t (loop for c of-type base-char = (pop chars) unless chars return t)))))
(define-test sacla-must-loop.238 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for i of-type (integer 0 3) = 0 then (1+ i)
           do (push i stack)
           when (= i 3) return t)
     stack)
   '(3 2 1 0))))
(define-test sacla-must-loop.239 (:tag :sacla)
 (assert-true
  (flet ((triple (n)
           (values n (+ n 1) (+ n 2))))
    (equal
     (loop for i from 0 upto 2
           for (a b c) = (multiple-value-list (triple i))
           append `(,a ,b ,c))
     '(0 1 2 1 2 3 2 3 4)))))
(define-test sacla-must-loop.240 (:tag :sacla)
 (assert-true
  (flet ((triple (n)
           (values n `(,(+ n 1)) `((,(+ n 2))))))
    (equal
     (loop for i from 0 upto 2
           for (a (b) ((c))) = (multiple-value-list (triple i))
           append `(,a ,b ,c))
     '(0 1 2 1 2 3 2 3 4)))))
(define-test sacla-must-loop.241 (:tag :sacla)
 (assert-true
  (flet ((triple (n)
           (values n
                   `(,(+ n 10) ,(+ n 11) ,(+ n 12) ,(+ n 13))
                   `(,(+ n 20) ,(+ n 21) ,(+ n 22)))))
    (equal
     (loop for i from 0 upto 2
           for (a (b0 b1 b2 b3) (c0 c1 c2)) = (multiple-value-list (triple i))
           append `(,a ,b0 ,b1 ,b2 ,b3 ,c0 ,c1 ,c2))
     '(0 10 11 12 13 20 21 22 1 11 12 13 14 21 22 23 2 12 13 14 15 22 23 24)))))
(define-test sacla-must-loop.242 (:tag :sacla)
 (assert-true
  (flet ((triple (n)
           (values n
                   `(,(+ n 10) ,(+ n 11) ,(+ n 12) ,(+ n 13))
                   `(,(+ n 200) (,(+ n 210) ,(+ n 211) ,(+ n 212) ,(+ n 213))
                     ,(+ n 220)))))
    (equal
     (loop for i from 0 upto 2
           for (a (b0 b1 b2 b3) (c0 (c10 c11 c12) c2)) =
               (multiple-value-list (triple i))
           append `(,a ,b0 ,b1 ,b2 ,b3 ,c0 ,c10 ,c11 ,c12 ,c2))
     '(0 10 11 12 13 200 210 211 212 220 1 11 12 13 14 201 211 212 213 221 2 12
       13 14 15 202 212 213 214 222)))))
(define-test sacla-must-loop.243 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for k being each hash-key of table do (push k stack))
    (null (set-difference stack '(k0 k1 k2))))))
(define-test sacla-must-loop.244 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for k being the hash-key of table do (push k stack))
    (null (set-difference stack '(k0 k1 k2))))))
(define-test sacla-must-loop.245 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for k being each hash-keys of table do (push k stack))
    (null (set-difference stack '(k0 k1 k2))))))
(define-test sacla-must-loop.246 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for k being the hash-keys of table do (push k stack))
    (null (set-difference stack '(k0 k1 k2))))))
(define-test sacla-must-loop.247 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for k being each hash-key in table do (push k stack))
    (null (set-difference stack '(k0 k1 k2))))))
(define-test sacla-must-loop.248 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for k being the hash-key in table do (push k stack))
    (null (set-difference stack '(k0 k1 k2))))))
(define-test sacla-must-loop.249 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for k being each hash-keys in table do (push k stack))
    (null (set-difference stack '(k0 k1 k2))))))
(define-test sacla-must-loop.250 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for k being the hash-keys in table do (push k stack))
    (null (set-difference stack '(k0 k1 k2))))))
(define-test sacla-must-loop.251 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for k being each hash-key of table using (hash-value v)
          do (push (list k v) stack))
    (null (set-difference stack '((k0 v0) (k1 v1) (k2 v2)) :test #'equal)))))
(define-test sacla-must-loop.252 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for k being the hash-key of table using (hash-value v)
          do (push (list k v) stack))
    (null (set-difference stack '((k0 v0) (k1 v1) (k2 v2)) :test #'equal)))))
(define-test sacla-must-loop.253 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for k being each hash-keys of table using (hash-value v)
          do (push (list k v) stack))
    (null (set-difference stack '((k0 v0) (k1 v1) (k2 v2)) :test #'equal)))))
(define-test sacla-must-loop.254 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for k being the hash-keys of table using (hash-value v)
          do (push (list k v) stack))
    (null (set-difference stack '((k0 v0) (k1 v1) (k2 v2)) :test #'equal)))))
(define-test sacla-must-loop.255 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for k being each hash-key in table using (hash-value v)
          do (push (list k v) stack))
    (null (set-difference stack '((k0 v0) (k1 v1) (k2 v2)) :test #'equal)))))
(define-test sacla-must-loop.256 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for k being the hash-key in table using (hash-value v)
          do (push (list k v) stack))
    (null (set-difference stack '((k0 v0) (k1 v1) (k2 v2)) :test #'equal)))))
(define-test sacla-must-loop.257 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for k being each hash-keys in table using (hash-value v)
          do (push (list k v) stack))
    (null (set-difference stack '((k0 v0) (k1 v1) (k2 v2)) :test #'equal)))))
(define-test sacla-must-loop.258 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for k being the hash-keys in table using (hash-value v)
          do (push (list k v) stack))
    (null (set-difference stack '((k0 v0) (k1 v1) (k2 v2)) :test #'equal)))))
(define-test sacla-must-loop.259 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for v being each hash-value of table do (push v stack))
    (null (set-exclusive-or stack '(v0 v1 v2))))))
(define-test sacla-must-loop.260 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for v being the hash-value of table do (push v stack))
    (null (set-exclusive-or stack '(v0 v1 v2))))))
(define-test sacla-must-loop.261 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for v being each hash-values of table do (push v stack))
    (null (set-exclusive-or stack '(v0 v1 v2))))))
(define-test sacla-must-loop.262 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for v being the hash-values of table do (push v stack))
    (null (set-exclusive-or stack '(v0 v1 v2))))))
(define-test sacla-must-loop.263 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for v being each hash-value in table do (push v stack))
    (null (set-exclusive-or stack '(v0 v1 v2))))))
(define-test sacla-must-loop.264 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for v being the hash-value in table do (push v stack))
    (null (set-exclusive-or stack '(v0 v1 v2))))))
(define-test sacla-must-loop.265 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for v being each hash-values in table do (push v stack))
    (null (set-exclusive-or stack '(v0 v1 v2))))))
(define-test sacla-must-loop.266 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for v being the hash-values in table do (push v stack))
    (null (set-exclusive-or stack '(v0 v1 v2))))))
(define-test sacla-must-loop.267 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for v being each hash-value of table using (hash-key k)
          do (push (list k v) stack))
    (null (set-exclusive-or stack '((k0 v0) (k1 v1) (k2 v2)) :test #'equal)))))
(define-test sacla-must-loop.268 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for v being the hash-value of table using (hash-key k)
          do (push (list k v) stack))
    (null (set-exclusive-or stack '((k0 v0) (k1 v1) (k2 v2)) :test #'equal)))))
(define-test sacla-must-loop.269 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for v being each hash-values of table using (hash-key k)
          do (push (list k v) stack))
    (null (set-exclusive-or stack '((k0 v0) (k1 v1) (k2 v2)) :test #'equal)))))
(define-test sacla-must-loop.270 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for v being the hash-values of table using (hash-key k)
          do (push (list k v) stack))
    (null (set-exclusive-or stack '((k0 v0) (k1 v1) (k2 v2)) :test #'equal)))))
(define-test sacla-must-loop.271 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for v being each hash-value in table using (hash-key k)
          do (push (list k v) stack))
    (null (set-exclusive-or stack '((k0 v0) (k1 v1) (k2 v2)) :test #'equal)))))
(define-test sacla-must-loop.272 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for v being the hash-value in table using (hash-key k)
          do (push (list k v) stack))
    (null (set-exclusive-or stack '((k0 v0) (k1 v1) (k2 v2)) :test #'equal)))))
(define-test sacla-must-loop.273 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for v being each hash-values in table using (hash-key k)
          do (push (list k v) stack))
    (null (set-exclusive-or stack '((k0 v0) (k1 v1) (k2 v2)) :test #'equal)))))
(define-test sacla-must-loop.274 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for v being the hash-values in table using (hash-key k)
          do (push (list k v) stack))
    (null (set-exclusive-or stack '((k0 v0) (k1 v1) (k2 v2)) :test #'equal)))))
(define-test sacla-must-loop.275 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table :test 'equal)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v))
          '((k0 k00) (k1 k11) (k2 k22))
          '((v0 v00) (v1 v11) (v2 v22)))
    (loop for (k kk) being each hash-key of table do (push (list k kk) stack))
    (null
     (set-exclusive-or stack '((k0 k00) (k1 k11) (k2 k22)) :test #'equal)))))
(define-test sacla-must-loop.276 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table :test 'equal)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v))
          '((k0 k00) (k1 k11) (k2 k22))
          '((v0 v00) (v1 v11) (v2 v22)))
    (loop :for (k kk) :being :each :hash-key :of table :using
               (hash-value (v vv))
          do (push (list k kk v vv) stack))
    (null
     (set-exclusive-or stack
                       '((k0 k00 v0 v00) (k1 k11 v1 v11) (k2 k22 v2 v22))
                       :test #'equal)))))
(define-test sacla-must-loop.277 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table :test 'equal)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v))
          '((k0 k00) (k1 k11) (k2 k22))
          '((v0 v00) (v1 v11) (v2 v22)))
    (loop :for (v vv) :being :each :hash-value :of table :using
               (hash-key (k kk))
          do (push (list k kk v vv) stack))
    (null
     (set-exclusive-or stack
                       '((k0 k00 v0 v00) (k1 k11 v1 v11) (k2 k22 v2 v22))
                       :test #'equal)))))
(define-test sacla-must-loop.278 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for k of-type symbol being each hash-key of table do (push k stack))
    (null (set-exclusive-or stack '(k0 k1 k2))))))
(define-test sacla-must-loop.279 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table :test 'equal)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v))
          '((k0 k00) (k1 k11) (k2 k22))
          '((v0 v00) (v1 v11) (v2 v22)))
    (loop for (k kk) of-type symbol being each hash-key of table
          do (push (list k kk) stack))
    (null
     (set-exclusive-or stack '((k0 k00) (k1 k11) (k2 k22)) :test #'equal)))))
(define-test sacla-must-loop.280 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table :test 'equal)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v))
          '((k0 k00) (k1 k11) (k2 k22))
          '((v0 v00) (v1 v11) (v2 v22)))
    (loop for (k kk) of-type (symbol symbol) being each hash-key of table
          do (push (list k kk) stack))
    (null
     (set-exclusive-or stack '((k0 k00) (k1 k11) (k2 k22)) :test #'equal)))))
(define-test sacla-must-loop.281 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(0 1 2) '(v0 v1 v2))
    (loop for k fixnum being each hash-key of table do (push k stack))
    (null (set-exclusive-or stack '(0 1 2))))))
(define-test sacla-must-loop.282 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(0 1 2) '(v0 v1 v2))
    (loop for k of-type fixnum being each hash-key of table do (push k stack))
    (null (set-exclusive-or stack '(0 1 2))))))
(define-test sacla-must-loop.283 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v))
          '(0.0 1.0 2.0)
          '(v0 v1 v2))
    (loop for k float being each hash-key of table do (push k stack))
    (null (set-exclusive-or stack '(0.0 1.0 2.0))))))
(define-test sacla-must-loop.284 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v))
          '(0.0 1.0 2.0)
          '(v0 v1 v2))
    (loop for k of-type float being each hash-key of table do (push k stack))
    (null (set-exclusive-or stack '(0.0 1.0 2.0))))))
(define-test sacla-must-loop.285 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v))
          '(0.0 1.0 2.0)
          '(v0 v1 v2))
    (loop for k t being each hash-key of table do (push k stack))
    (null (set-exclusive-or stack '(0.0 1.0 2.0))))))
(define-test sacla-must-loop.286 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v))
          '(0.0 1.0 2.0)
          '(v0 v1 v2))
    (loop for k of-type t being each hash-key of table do (push k stack))
    (null (set-exclusive-or stack '(0.0 1.0 2.0))))))
(define-test sacla-must-loop.287 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v))
          '(#\a #\b #\c)
          '(v0 v1 v2))
    (loop for k of-type character being each hash-key of table
          do (push k stack))
    (null (set-exclusive-or stack '(#\a #\b #\c))))))
(define-test sacla-must-loop.288 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for v t being each hash-value of table do (push v stack))
    (null (set-exclusive-or stack '(v0 v1 v2))))))
(define-test sacla-must-loop.289 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for v of-type t being each hash-value of table do (push v stack))
    (null (set-exclusive-or stack '(v0 v1 v2))))))
(define-test sacla-must-loop.290 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(v0 v1 v2))
    (loop for v of-type symbol being each hash-value of table
          do (push v stack))
    (null (set-exclusive-or stack '(v0 v1 v2))))))
(define-test sacla-must-loop.291 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(0 1 2))
    (loop for v fixnum being each hash-value of table do (push v stack))
    (null (set-exclusive-or stack '(0 1 2))))))
(define-test sacla-must-loop.292 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v)) '(k0 k1 k2) '(0 1 2))
    (loop for v of-type (integer 0 2) being each hash-value of table
          do (push v stack))
    (null (set-exclusive-or stack '(0 1 2))))))
(define-test sacla-must-loop.293 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v))
          '(k0 k1 k2)
          '(0.0 1.0 2.0))
    (loop for v float being each hash-value of table do (push v stack))
    (null (set-exclusive-or stack '(0.0 1.0 2.0))))))
(define-test sacla-must-loop.294 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v))
          '(k0 k1 k2)
          '(#\a #\b #\c))
    (loop for v of-type base-char being each hash-value of table
          do (push v stack))
    (null (set-exclusive-or stack '(#\a #\b #\c))))))
(define-test sacla-must-loop.295 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a from 1 upto 3 and x = 0 then a do (push x stack))
     stack)
   '(2 1 0))))
(define-test sacla-must-loop.296 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a from 0 upto 3 for x = 0 then a do (push x stack))
     stack)
   '(3 2 1 0))))
(define-test sacla-must-loop.297 (:tag :sacla)
 (assert-true
  (equal
   (let ((i 4) stack)
     (loop for a = 0 then (1+ a)
           for b = 0 then a
           for c = 0 then b
           do (when (zerop (decf i))
                (return))
              (push (list a b c) stack))
     stack)
   '((2 2 2) (1 1 1) (0 0 0)))))
(define-test sacla-must-loop.298 (:tag :sacla)
 (assert-true
  (equal
   (let ((i 5) stack)
     (loop for a = 0 then (1+ a)
           and b = 0 then a
           and c = 0 then b
           do (when (zerop (decf i))
                (return))
              (push (list a b c) stack))
     stack)
   '((3 2 1) (2 1 0) (1 0 0) (0 0 0)))))
(define-test sacla-must-loop.299 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a in '(0 1 2 3) for x = a do (push x stack))
     stack)
   '(3 2 1 0))))
(define-test sacla-must-loop.300 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a in '(0 1 2 3) and x = 100 then a do (push x stack))
     stack)
   '(2 1 0 100))))
(define-test sacla-must-loop.301 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a on '(0 1 2 3) for x = (car a) do (push x stack))
     stack)
   '(3 2 1 0))))
(define-test sacla-must-loop.302 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a on '(0 1 2 3) and x = 100 then (car a) do (push x stack))
     stack)
   '(2 1 0 100))))
(define-test sacla-must-loop.303 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a across #(0 1 2 3) for x = a do (push x stack))
     stack)
   '(3 2 1 0))))
(define-test sacla-must-loop.304 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for a across #(0 1 2 3) and x = 100 then a do (push x stack))
     stack)
   '(2 1 0 100))))
(define-test sacla-must-loop.305 (:tag :sacla)
 (assert-true
  (equal (loop for x from 1 to 10 for y = nil then x collect (list x y))
         '((1 nil) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (8 8) (9 9) (10 10)))))
(define-test sacla-must-loop.306 (:tag :sacla)
 (assert-true
  (equal (loop for x from 1 to 10 and y = nil then x collect (list x y))
         '((1 nil) (2 1) (3 2) (4 3) (5 4) (6 5) (7 6) (8 7) (9 8) (10 9)))))
(define-test sacla-must-loop.307 (:tag :sacla)
 (assert-true
  (= 280
     (loop for a upfrom 0 upto 9
           and b downfrom 9 downto 0
           and c from 0 to 9
           and d from 10 above 0
           and e below 10
           and f to 9
           summing (+ a b c d e f)))))
(define-test sacla-must-loop.308 (:tag :sacla)
 (assert-true
  (equal
   (loop for a from 1 upto 9
         as b = 0 then a
         as c = -1 then b
         as d = -2 then c
         as e = -3 then d
         as f = -4 then e
         collecting (list a b c d e f))
   '((1 0 -1 -2 -3 -4) (2 2 2 2 2 2) (3 3 3 3 3 3) (4 4 4 4 4 4) (5 5 5 5 5 5)
     (6 6 6 6 6 6) (7 7 7 7 7 7) (8 8 8 8 8 8) (9 9 9 9 9 9)))))
(define-test sacla-must-loop.309 (:tag :sacla)
 (assert-true
  (equal
   (loop for a from 1 upto 9
         and b = 0 then a
         and c = -1 then b
         and d = -2 then c
         and e = -3 then d
         and f = -4 then e
         collecting (list a b c d e f))
   '((1 0 -1 -2 -3 -4) (2 1 0 -1 -2 -3) (3 2 1 0 -1 -2) (4 3 2 1 0 -1)
     (5 4 3 2 1 0) (6 5 4 3 2 1) (7 6 5 4 3 2) (8 7 6 5 4 3) (9 8 7 6 5 4)))))
(define-test sacla-must-loop.310 (:tag :sacla)
 (assert-true
  (equal
   (loop for a from 1 upto 9
         and b = 0 then a
         and c = -1 then b
         and d = -2 then c
         and e = -3 then d
         and f = -4 then e
         for i from 9 downto 1
         and j = 8 then i
         and k = 7 then j
         and l = 6 then k
         and m = 5 then l
         and n = 4 then m
         collecting (list a b c d e f)
         collecting (list i j k l m n))
   '((1 0 -1 -2 -3 -4) (9 8 7 6 5 4) (2 1 0 -1 -2 -3) (8 9 8 7 6 5)
     (3 2 1 0 -1 -2) (7 8 9 8 7 6) (4 3 2 1 0 -1) (6 7 8 9 8 7) (5 4 3 2 1 0)
     (5 6 7 8 9 8) (6 5 4 3 2 1) (4 5 6 7 8 9) (7 6 5 4 3 2) (3 4 5 6 7 8)
     (8 7 6 5 4 3) (2 3 4 5 6 7) (9 8 7 6 5 4) (1 2 3 4 5 6)))))
(define-test sacla-must-loop.311 (:tag :sacla)
 (assert-true
  (let (stack)
    (loop for a on
              (progn
                (push 1 stack)
                '(0 1 2))
          and b across
              (progn
                (push 2 stack)
                "abc"))
    (equal '(2 1) stack))))
(define-test sacla-must-loop.312 (:tag :sacla)
 (assert-true
  (equal
   (let ((a 5))
     (loop for a from 0 upto 5 and b from a downto 0 collect (list a b)))
   '((0 5) (1 4) (2 3) (3 2) (4 1) (5 0)))))
(define-test sacla-must-loop.313 (:tag :sacla)
 (assert-true
  (equal
   (let ((a :outer))
     (loop for a from 0 upto 5 and b in (list a) collect (list a b)))
   '((0 :outer)))))
(define-test sacla-must-loop.314 (:tag :sacla)
 (assert-true
  (equal
   (let ((b 0))
     (loop for a from b upto 5 and b in '(a b c) collecting (list a b)))
   '((0 a) (1 b) (2 c)))))
(define-test sacla-must-loop.315 (:tag :sacla)
 (assert-true (zerop (loop with x = 0 do (return x)))))
(define-test sacla-must-loop.316 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop with x = 1 for a from x to 3 by 1 do (push a stack))
     stack)
   '(3 2 1))))
(define-test sacla-must-loop.317 (:tag :sacla)
 (assert-true
  (equal
   (loop with a = 1 with b = (+ a 2) with c = (+ b 3) return (list a b c))
   '(1 3 6))))
(define-test sacla-must-loop.318 (:tag :sacla)
 (assert-true
  (equal (loop with a = 1 and b = 2 and c = 3 return (list a b c)) '(1 2 3))))
(define-test sacla-must-loop.319 (:tag :sacla)
 (assert-true
  (let ((a 5) (b 10))
    (equal
     (loop with a = 1 and b = (+ a 2) and c = (+ b 3) return (list a b c))
     '(1 7 13)))))
(define-test sacla-must-loop.320 (:tag :sacla)
 (assert-true
  (equal (loop with (a b c) of-type (float integer float) return (list a b c))
         '(0.0 0 0.0))))
(define-test sacla-must-loop.321 (:tag :sacla)
 (assert-true
  (equal (loop with (a b c) of-type float return (list a b c)) '(0.0 0.0 0.0))))
(define-test sacla-must-loop.322 (:tag :sacla)
 (assert-true
  (flet ((triple ()
           (values 0 1 2)))
    (equal
     (loop with (a b c) = (multiple-value-list (triple))
           do (return (list a b c)))
     '(0 1 2)))))
(define-test sacla-must-loop.323 (:tag :sacla)
 (assert-true
  (flet ((triple ()
           (values 0 '(1) 2)))
    (equal
     (loop with (a (b) c) = (multiple-value-list (triple))
           do (return (list a b c)))
     '(0 1 2)))))
(define-test sacla-must-loop.324 (:tag :sacla)
 (assert-true
  (flet ((triple ()
           (values 0 '(0 1 2) 2)))
    (equal
     (loop with (a (nil b) c d) = (multiple-value-list (triple))
           do (return (list a b c d)))
     '(0 1 2 nil)))))
(define-test sacla-must-loop.325 (:tag :sacla)
 (assert-true
  (flet ((triple ()
           (values 0 1 2)))
    (equal
     (loop with (a b c) fixnum = (multiple-value-list (triple))
           do (return (list a b c)))
     '(0 1 2)))))
(define-test sacla-must-loop.326 (:tag :sacla)
 (assert-true
  (flet ((triple ()
           (values 0 '(1) 2)))
    (equal
     (loop with (a (b) c) of-type (fixnum (fixnum) fixnum) =
                (multiple-value-list (triple))
           do (return (list a b c)))
     '(0 1 2)))))
(define-test sacla-must-loop.327 (:tag :sacla)
 (assert-true
  (equal (loop for a from 0 upto 5 for b from a downto -5 collect (list a b))
         '((0 0) (1 -1) (2 -2) (3 -3) (4 -4) (5 -5)))))
(define-test sacla-must-loop.328 (:tag :sacla)
 (assert-true
  (equal (loop for a from 0 upto 5 with x = a collect (list a x))
         '((0 0) (1 0) (2 0) (3 0) (4 0) (5 0)))))
(define-test sacla-must-loop.329 (:tag :sacla)
 (assert-true (zerop (loop initially (return 0)))))
(define-test sacla-must-loop.330 (:tag :sacla)
 (assert-true (zerop (loop repeat 2 finally (return 0)))))
(define-test sacla-must-loop.331 (:tag :sacla)
 (assert-true (= (loop with x = 0 initially (incf x) return x) 1)))
(define-test sacla-must-loop.332 (:tag :sacla)
 (assert-true
  (=
   (loop with x = 0
         for a from 0 below 3
         initially (incf x)
         finally (return (incf x)))
   2)))
(define-test sacla-must-loop.333 (:tag :sacla)
 (assert-true
  (=
   (loop with x = 0
         for a from 0 below 3
         initially (incf x)
                   (incf x)
         finally (return (incf x)))
   3)))
(define-test sacla-must-loop.334 (:tag :sacla)
 (assert-true
  (=
   (loop with x = 0
         for a from 0 upto 3
         initially (incf x)
         finally (incf x)
                 (return (incf x)))
   3)))
(define-test sacla-must-loop.335 (:tag :sacla)
 (assert-true
  (=
   (loop with x = 0
         for a from 0 upto 3
         initially (incf x)
                   (incf x)
         finally (incf x)
                 (return (incf x)))
   4)))
(define-test sacla-must-loop.336 (:tag :sacla)
 (assert-true
  (=
   (loop with x = 0
         for a from 0 below 3
         do (incf x)
         initially (incf x)
                   (incf x)
         finally (incf x)
                 (return (incf x)))
   7)))
(define-test sacla-must-loop.337 (:tag :sacla)
 (assert-true
  (= 33
     (loop with x = 2
           initially (setq x (* x 3))
           for i below 3
           initially (setq x (* x 5))
           do (incf x i)
           finally (return x)))))
(define-test sacla-must-loop.338 (:tag :sacla)
 (assert-true
  (equal
   (loop with x = nil
         repeat 2
         initially (push 'initially0 x)
         finally (push 'finally0 x)
         initially (push 'initially1 x)
         finally (push 'finally1 x)
         do (push 'body0 x)
         finally (push 'finally2 x)
                 (push 'finally3 x)
         finally (return (reverse x))
         initially (push 'initially2 x)
                   (push 'initially3 x)
         do (push 'body1 x))
   '(initially0 initially1 initially2 initially3 body0 body1 body0 body1
     finally0 finally1 finally2 finally3))))
(define-test sacla-must-loop.339 (:tag :sacla)
 (assert-true
  (equal
   (loop with i = 3
         with stack = nil
         do (when (zerop i)
              (loop-finish))
            (decf i)
            (push i stack)
         finally (return stack))
   '(0 1 2))))
(define-test sacla-must-loop.340 (:tag :sacla)
 (assert-true
  (equal
   (loop with i = 3
         with stack = nil
         doing (when (zerop i)
                 (loop-finish))
               (decf i)
               (push i stack)
         finally (return stack))
   '(0 1 2))))
(define-test sacla-must-loop.341 (:tag :sacla)
 (assert-true (= (loop with x = 10 do (return x)) 10)))
(define-test sacla-must-loop.342 (:tag :sacla)
 (assert-true (= (loop with x = 10 doing (return x)) 10)))
(define-test sacla-must-loop.343 (:tag :sacla)
 (assert-true
  (=
   (loop with x = 0
         do (incf x)
         doing (incf x)
               (return x))
   2)))
(define-test sacla-must-loop.344 (:tag :sacla)
 (assert-true (= (loop with x = 0 do (incf x) doing (incf x) do (return x)) 2)))
(define-test sacla-must-loop.345 (:tag :sacla)
 (assert-true
  (=
   (loop with x = 0
         do (incf x)
            (incf x)
         doing (return x))
   2)))
(define-test sacla-must-loop.346 (:tag :sacla)
 (assert-true
  (=
   (loop with x = 0
         do (incf x)
            (incf x)
            (incf x)
         doing (incf x)
               (return x))
   4)))
(define-test sacla-must-loop.347 (:tag :sacla)
 (assert-true
  (let ((odd 0) (even 0))
    (and
     (null
      (loop for a from 1 upto 10
            if (oddp a) do (incf odd) else do (incf even) end))
     (= 5 odd even)))))
(define-test sacla-must-loop.348 (:tag :sacla)
 (assert-true
  (let ((odd+ 0) (even+ 0) (odd- 0) (even- 0))
    (and
     (null
      (loop for a from -10 upto 10
            if (oddp a)
              if (> a 0) do (incf odd+) else do (incf odd-) end
            else
              if (> a 0) do (incf even+) else do (incf even-)))
     (= 5 odd+ even+ odd-)
     (= even- 6)))))
(define-test sacla-must-loop.349 (:tag :sacla)
 (assert-true
  (let ((odd+ 0) (even+ 0) (odd- 0) (even- 0))
    (and
     (null
      (loop for a from -10 upto 10
            unless (zerop a)
              if (oddp a)
                if (> a 0) do (incf odd+) else do (incf odd-) end
              else
                if (> a 0) do (incf even+) else do (incf even-)))
     (= 5 odd+ even+ odd- even-)))))
(define-test sacla-must-loop.350 (:tag :sacla)
 (assert-true
  (let ((odd+ 0) (even+ 0) (odd- 0) (even- 0))
    (and
     (null
      (loop for a from -10 upto 10
            if (not (zerop a))
              when (oddp a)
                unless (< a 0) do (incf odd+) else do (incf odd-) end
              else
                unless (<= a 0) do (incf even+) else do (incf even-)))
     (= 5 odd+ even+ odd- even-)))))
(define-test sacla-must-loop.351 (:tag :sacla)
 (assert-true
  (handler-bind ((simple-error #'(lambda (c) (declare (ignore c)) (continue))))
    (eq 'continued
        (loop for item in '(1 2 3 a 4 5)
              when (not (numberp item))
                return (or
                        (cerror "ignore this error"
                                "non-numeric value: ~s"
                                item)
                        'continued))))))
(define-test sacla-must-loop.352 (:tag :sacla)
 (assert-true
  (equal
   (loop for i in '(1 324 2345 323 2 4 235 252)
         when (oddp i)
           collect i into odd-numbers
         else
           collect i into even-numbers
         finally (return (list odd-numbers even-numbers)))
   '((1 2345 323 235) (324 2 4 252)))))
(define-test sacla-must-loop.353 (:tag :sacla)
 (assert-true
  (equal (loop for i in '(1 2 3 4 5 6) when (and (> i 3) i) collect it)
         '(4 5 6))))
(define-test sacla-must-loop.354 (:tag :sacla)
 (assert-true
  (= 4 (loop for i in '(1 2 3 4 5 6) when (and (> i 3) i) return it))))
(define-test sacla-must-loop.355 (:tag :sacla)
 (assert-true
  (equal
   (let ((list '(0 3.0 apple 4 5 9.8 orange banana)))
     (loop for i in list
           when (numberp i)
             when (floatp i)
               collect i into float-numbers
             else
               collect i into other-numbers
           else
             when (symbolp i)
               collect i into symbol-list
             else
               do (error "found a funny value in list ~S, value ~S~%" list i)
           finally (return (list float-numbers other-numbers symbol-list))))
   '((3.0 9.8) (0 4 5) (apple orange banana)))))
(define-test sacla-must-loop.356 (:tag :sacla)
 (assert-true (equal (loop for i below 5 if (oddp i) collecting i) '(1 3))))
(define-test sacla-must-loop.357 (:tag :sacla)
 (assert-true (equal (loop for i below 5 when (oddp i) collecting i) '(1 3))))
(define-test sacla-must-loop.358 (:tag :sacla)
 (assert-true
  (equal (loop for i below 5 if (oddp i) collecting i else collecting (list i))
         '((0) 1 (2) 3 (4)))))
(define-test sacla-must-loop.359 (:tag :sacla)
 (assert-true
  (equal
   (loop for i below 5 when (oddp i) collecting i else collecting (list i))
   '((0) 1 (2) 3 (4)))))
(define-test sacla-must-loop.360 (:tag :sacla)
 (assert-true
  (equal (loop for i below 5 unless (evenp i) collecting i) '(1 3))))
(define-test sacla-must-loop.361 (:tag :sacla)
 (assert-true
  (equal
   (loop for i below 5 unless (evenp i) collecting i else collecting (list i))
   '((0) 1 (2) 3 (4)))))
(define-test sacla-must-loop.362 (:tag :sacla)
 (assert-true (equal (loop for i below 5 if (oddp i) collecting i end) '(1 3))))
(define-test sacla-must-loop.363 (:tag :sacla)
 (assert-true
  (equal (loop for i below 5 when (oddp i) collecting i end) '(1 3))))
(define-test sacla-must-loop.364 (:tag :sacla)
 (assert-true
  (equal
   (loop for i below 5 if (oddp i) collecting i else collecting (list i) end)
   '((0) 1 (2) 3 (4)))))
(define-test sacla-must-loop.365 (:tag :sacla)
 (assert-true
  (equal
   (loop for i below 5 when (oddp i) collecting i else collecting (list i) end)
   '((0) 1 (2) 3 (4)))))
(define-test sacla-must-loop.366 (:tag :sacla)
 (assert-true
  (equal (loop for i below 5 unless (evenp i) collecting i end) '(1 3))))
(define-test sacla-must-loop.367 (:tag :sacla)
 (assert-true
  (equal
   (loop for i below 5
         unless (evenp i) collecting i else collecting (list i) end)
   '((0) 1 (2) 3 (4)))))
(define-test sacla-must-loop.368 (:tag :sacla)
 (assert-true
  (equal
   (loop for (a b) in '((0 0) (0 1))
         if (zerop a) if (zerop b) collect '|0-0| else collect '|0-1|)
   '(|0-0| |0-1|))))
(define-test sacla-must-loop.369 (:tag :sacla)
 (assert-true
  (equal
   (loop for (a b) in '((0 0) (0 1))
         when (zerop a) if (zerop b) collect '|0-0| else collect '|0-1|)
   '(|0-0| |0-1|))))
(define-test sacla-must-loop.370 (:tag :sacla)
 (assert-true
  (equal
   (loop for (a b) in '((0 0) (0 1) (1 0) (1 1))
         if (zerop a) if (= b 1) collect '|0-1| end else collect '|1-X|)
   '(|0-1| |1-X| |1-X|))))
(define-test sacla-must-loop.371 (:tag :sacla)
 (assert-true
  (equal
   (loop for (a b) in '((0 0) (0 1) (1 0) (1 1))
         when (zerop a) if (= b 1) collect '|0-1| end else collect '|1-X|)
   '(|0-1| |1-X| |1-X|))))
(define-test sacla-must-loop.372 (:tag :sacla)
 (assert-true
  (equal
   (loop for (a b) in '((0 0) (0 1))
         unless (not (zerop a))
           if (zerop b) collect '|0-0| else collect '|0-1|)
   '(|0-0| |0-1|))))
(define-test sacla-must-loop.373 (:tag :sacla)
 (assert-true
  (equal
   (loop for (a b) in '((0 0) (0 1) (1 0) (1 1))
         unless (not (zerop a))
           if (= b 1) collect '|0-1| end
         else
           collect '|1-X|)
   '(|0-1| |1-X| |1-X|))))
(define-test sacla-must-loop.374 (:tag :sacla)
 (assert-true
  (equal
   (loop for (a b c) in
             '((0 0 0) (0 0 1) (0 1 0) (0 1 1) (1 0 0) (1 0 1) (1 1 0) (1 1 1))
         if (zerop a)
           if (zerop b)
             if (zerop c) collect 'x0-0-0 else collect 'x0-0-1
           else
             if (zerop c) collect 'x0-1-0 else collect 'x0-1-1
         else
           if (zerop b)
             if (zerop c) collect 'x1-0-0 else collect 'x1-0-1
           else
             if (zerop c) collect 'x1-1-0 else collect 'x1-1-1)
   '(x0-0-0 x0-0-1 x0-1-0 x0-1-1 x1-0-0 x1-0-1 x1-1-0 x1-1-1))))
(define-test sacla-must-loop.375 (:tag :sacla)
 (assert-true
  (equal
   (loop for a below 10
         if (oddp a)
           collect a into bag
           and sum a into odd
         else
           collect (list a) into bag
           and sum a into even
         finally (return (list bag odd even)))
   '(((0) 1 (2) 3 (4) 5 (6) 7 (8) 9) 25 20))))
(define-test sacla-must-loop.376 (:tag :sacla)
 (assert-true
  (equal
   (loop for a below 10
         if (oddp a)
           collect a
           and collect (list a)
           and collect (list (list a))
         else
           collect a)
   '(0 1 (1) ((1)) 2 3 (3) ((3)) 4 5 (5) ((5)) 6 7 (7) ((7)) 8 9 (9) ((9))))))
(define-test sacla-must-loop.377 (:tag :sacla)
 (assert-true
  (let ((c0 0) (c1 0))
    (and
     (equal
      (loop for a below 10
            when (oddp a)
              collect a
              and do (incf c0)
                     (decf c1)
              and collect (list a))
      '(1 (1) 3 (3) 5 (5) 7 (7) 9 (9)))
     (= c0 5)
     (= c1 -5)))))
(define-test sacla-must-loop.378 (:tag :sacla)
 (assert-true (zerop (loop return 0))))
(define-test sacla-must-loop.379 (:tag :sacla)
 (assert-true
  (= (loop for a from 0 below 3 when (and (oddp a) a) return it) 1)))
(define-test sacla-must-loop.380 (:tag :sacla)
 (assert-true (eq (loop for a in '(nil nil ok nil ok2) when a return it) 'ok)))
(define-test sacla-must-loop.381 (:tag :sacla)
 (assert-true (eq 'ok (loop with a = 'ok if a return it else return it))))
(define-test sacla-must-loop.382 (:tag :sacla)
 (assert-true
  (equal (multiple-value-list (loop return (values 0 1 2))) '(0 1 2))))
(define-test sacla-must-loop.383 (:tag :sacla)
 (assert-true
  (let ((flag nil))
    (and
     (eq t (loop for a below 3 when (oddp a) return t finally (setq flag t)))
     (not flag)))))
(define-test sacla-must-loop.384 (:tag :sacla)
 (assert-true
  (equal
   (loop for a in '(0 1 2 3)
         and b in '(3 2 1 0)
         if (and (oddp a) a)
           if (and (evenp b) b) when (and (= (* a b) 0) (list a b)) return it)
   '(3 0))))
(define-test sacla-must-loop.385 (:tag :sacla)
 (assert-true (equal (loop for a from 0 below 3 collect a) '(0 1 2))))
(define-test sacla-must-loop.386 (:tag :sacla)
 (assert-true (equal (loop for a from 0 below 3 collecting a) '(0 1 2))))
(define-test sacla-must-loop.387 (:tag :sacla)
 (assert-true
  (equal (loop for a in '(nil 0 nil nil 1 2 nil 3 nil 4) when a collect it)
         '(0 1 2 3 4))))
(define-test sacla-must-loop.388 (:tag :sacla)
 (assert-true
  (equal (loop for a in '(nil 0 nil nil 1 2 nil 3 nil 4) when a collecting it)
         '(0 1 2 3 4))))
(define-test sacla-must-loop.389 (:tag :sacla)
 (assert-true
  (equal
   (loop for a in '(nil 0 nil nil 1 2 nil 3 nil 4)
         when a collect it into bag
         finally (return bag))
   '(0 1 2 3 4))))
(define-test sacla-must-loop.390 (:tag :sacla)
 (assert-true
  (equal
   (loop for a in '(nil 0 nil nil 1 2 nil 3 nil 4)
         when a collecting it into bag
         finally (return bag))
   '(0 1 2 3 4))))
(define-test sacla-must-loop.391 (:tag :sacla)
 (assert-true
  (equal
   (loop for a below 10
         if (oddp a) collect a into odd else collect a into even end
         finally (return (list odd even)))
   '((1 3 5 7 9) (0 2 4 6 8)))))
(define-test sacla-must-loop.392 (:tag :sacla)
 (assert-true
  (equal (loop for a below 3 for b on '(2 1 0) collecting a appending b)
         '(0 2 1 0 1 1 0 2 0))))
(define-test sacla-must-loop.393 (:tag :sacla)
 (assert-true (= 15 (loop for i of-type fixnum in '(1 2 3 4 5) sum i))))
(define-test sacla-must-loop.394 (:tag :sacla)
 (assert-true
  (= 22.4
     (let ((series '(1.2 4.3 5.7)))
       (loop for v in series sum (* 2.0 v))))))
(define-test sacla-must-loop.395 (:tag :sacla)
 (assert-true
  (equal
   (loop for a below 10
         if (oddp a)
           collect a into odd
           and sum a into
         sum
         finally (return (list odd sum)))
   '((1 3 5 7 9) 25))))
(define-test sacla-must-loop.396 (:tag :sacla)
 (assert-true
  (equal
   (loop for a below 10
         if (oddp a)
           collect a into odd
           and sum a into odd-sum
         else
           collect a into even
           and sum a into even-sum
         end
         finally (return (list odd odd-sum even even-sum)))
   '((1 3 5 7 9) 25 (0 2 4 6 8) 20))))
(define-test sacla-must-loop.397 (:tag :sacla)
 (assert-true
  (equal
   (loop for i in '(bird 3 4 turtle (1 . 4) horse cat)
         when (symbolp i) collect i)
   '(bird turtle horse cat))))
(define-test sacla-must-loop.398 (:tag :sacla)
 (assert-true
  (equal (loop for i below 3 for j upto 2 collecting i collecting j)
         '(0 0 1 1 2 2))))
(define-test sacla-must-loop.399 (:tag :sacla)
 (assert-true
  (equal (loop for a from -10 upto 0 collecting a)
         '(-10 -9 -8 -7 -6 -5 -4 -3 -2 -1 0))))
(define-test sacla-must-loop.400 (:tag :sacla)
 (assert-true (null (loop for a from -10 upto 0 collecting a into list))))
(define-test sacla-must-loop.401 (:tag :sacla)
 (assert-true
  (let* ((zero (list 0))
         (one (list 1))
         (two (list 2))
         (list (list zero one two)))
    (and (equal (loop for a in list append a) '(0 1 2))
         (equal zero '(0))
         (equal one '(1))
         (equal two '(2))))))
(define-test sacla-must-loop.402 (:tag :sacla)
 (assert-true
  (equal (loop for a in '(nil (1) nil (2)) when a append a) '(1 2))))
(define-test sacla-must-loop.403 (:tag :sacla)
 (assert-true
  (equal (loop for a in '(nil (1) nil (2)) when a appending a) '(1 2))))
(define-test sacla-must-loop.404 (:tag :sacla)
 (assert-true (null (loop for a in '(nil (1) nil (2)) when a append a into x))))
(define-test sacla-must-loop.405 (:tag :sacla)
 (assert-true
  (null (loop for a in '(nil (1) nil (2)) when a appending a into x))))
(define-test sacla-must-loop.406 (:tag :sacla)
 (assert-true
  (equal
   (loop for a in '(nil (1) nil (2)) when a append a into x finally (return x))
   '(1 2))))
(define-test sacla-must-loop.407 (:tag :sacla)
 (assert-true
  (equal
   (loop for a in '(nil (1) nil (2))
         when a appending a into x
         finally (return x))
   '(1 2))))
(define-test sacla-must-loop.408 (:tag :sacla)
 (assert-true
  (equal (loop for a in '(nil (1) nil (2)) when a append it) '(1 2))))
(define-test sacla-must-loop.409 (:tag :sacla)
 (assert-true
  (equal (loop for a in '(nil (1) nil (2)) when a appending it) '(1 2))))
(define-test sacla-must-loop.410 (:tag :sacla)
 (assert-true
  (equal (loop for a on (list 0 1 2 3 4) when (oddp (car a)) append a)
         '(1 2 3 4 3 4))))
(define-test sacla-must-loop.411 (:tag :sacla)
 (assert-true
  (equal (loop for a on (list 0 1 2 3 4) when (oddp (car a)) appending a)
         '(1 2 3 4 3 4))))
(define-test sacla-must-loop.412 (:tag :sacla)
 (assert-true (equal (loop for x in '((a) (b) ((c))) append x) '(a b (c)))))
(define-test sacla-must-loop.413 (:tag :sacla)
 (assert-true
  (let ((list (list (list 0) (list 1) (list 2) (list 3))))
    (and (equal (loop for a in list nconc a) '(0 1 2 3))
         (equal list '((0 1 2 3) (1 2 3) (2 3) (3)))))))
(define-test sacla-must-loop.414 (:tag :sacla)
 (assert-true
  (let ((list (list (list 0) (list 1) (list 2) (list 3))))
    (and (equal (loop for a in list nconcing a) '(0 1 2 3))
         (equal list '((0 1 2 3) (1 2 3) (2 3) (3)))))))
(define-test sacla-must-loop.415 (:tag :sacla)
 (assert-true
  (let ((list (list nil (list 0) nil nil (list 1) (list 2) nil (list 3) nil)))
    (and (equal (loop for a in list when a nconc it) '(0 1 2 3))
         (equal list '(nil (0 1 2 3) nil nil (1 2 3) (2 3) nil (3) nil))))))
(define-test sacla-must-loop.416 (:tag :sacla)
 (assert-true
  (let ((list (list nil (list 0) nil nil (list 1) (list 2) nil (list 3) nil)))
    (and (equal (loop for a in list when a nconcing it) '(0 1 2 3))
         (equal list '(nil (0 1 2 3) nil nil (1 2 3) (2 3) nil (3) nil))))))
(define-test sacla-must-loop.417 (:tag :sacla)
 (assert-true
  (null
   (loop for a in (list (list (list 0) (list 1) (list 2) (list 3)))
         nconc a into x))))
(define-test sacla-must-loop.418 (:tag :sacla)
 (assert-true
  (null
   (loop for a in (list (list (list 0) (list 1) (list 2) (list 3)))
         nconcing a into x))))
(define-test sacla-must-loop.419 (:tag :sacla)
 (assert-true
  (let ((list (list (list 0) (list 1) (list 2) (list 3))))
    (and
     (equal (loop for a in list nconc a into x finally (return x)) '(0 1 2 3))
     (equal list '((0 1 2 3) (1 2 3) (2 3) (3)))))))
(define-test sacla-must-loop.420 (:tag :sacla)
 (assert-true
  (let ((list (list (list 0) (list 1) (list 2) (list 3))))
    (and
     (equal (loop for a in list nconcing a into x finally (return x))
            '(0 1 2 3))
     (equal list '((0 1 2 3) (1 2 3) (2 3) (3)))))))
(define-test sacla-must-loop.421 (:tag :sacla)
 (assert-true
  (equal
   (loop for i upfrom 0 as x in '(a b (c)) nconc (if (evenp i) (list x) nil))
   '(a (c)))))
(define-test sacla-must-loop.422 (:tag :sacla)
 (assert-true
  (equal
   (loop for a in '(0 3 6)
         for b in '((1) (4) (7))
         for c in (copy-tree '((2) (5) (8)))
         collecting a
         appending b
         nconcing c)
   '(0 1 2 3 4 5 6 7 8))))
(define-test sacla-must-loop.423 (:tag :sacla)
 (assert-true
  (equal
   (loop for a in '(0 3 6)
         for b in (copy-tree '((1) (4) (7)))
         for c in (list (list 2) (list 5) (list 8))
         collecting a
         nconcing b
         appending c)
   '(0 1 2 3 4 5 6 7 8))))
(define-test sacla-must-loop.424 (:tag :sacla)
 (assert-true
  (equal
   (loop for a in '((0) (3) (6))
         for b in (copy-tree '((1) (4) (7)))
         for c in '(2 5 8)
         appending a
         nconcing b
         collecting c)
   '(0 1 2 3 4 5 6 7 8))))
(define-test sacla-must-loop.425 (:tag :sacla)
 (assert-true
  (equal
   (loop for a in '((0) (3) (6))
         for b in '(1 4 7)
         for c in (copy-tree '((2) (5) (8)))
         appending a
         collecting b
         nconcing c)
   '(0 1 2 3 4 5 6 7 8))))
(define-test sacla-must-loop.426 (:tag :sacla)
 (assert-true
  (equal
   (loop for a in (copy-tree '((0) (3) (6)))
         for b in '(1 4 7)
         for c in '((2) (5) (8))
         nconcing a
         collecting b
         appending c)
   '(0 1 2 3 4 5 6 7 8))))
(define-test sacla-must-loop.427 (:tag :sacla)
 (assert-true
  (equal
   (loop for a in (copy-tree '((0) (3) (6)))
         for b in '((1) (4) (7))
         for c in '(2 5 8)
         nconcing a
         appending b
         collecting c)
   '(0 1 2 3 4 5 6 7 8))))
(define-test sacla-must-loop.428 (:tag :sacla)
 (assert-true
  (equal
   (loop for a in '(0 6)
         for b in '((1 2 3) (7 8 9))
         for c in (copy-tree '((4 5) (10)))
         collect a
         append b
         nconc c)
   '(0 1 2 3 4 5 6 7 8 9 10))))
(define-test sacla-must-loop.429 (:tag :sacla)
 (assert-true
  (null
   (loop for a in 'nil
         for b in '((1 2 3) (7 8 9))
         for c in (copy-tree '((4 5) (10)))
         collect a
         append b
         nconc c))))
(define-test sacla-must-loop.430 (:tag :sacla)
 (assert-true
  (equal
   (loop for a in '(0 3 6)
         for b in '((1) (4) (7))
         for c in (copy-tree '((2) (5) (8)))
         collecting a into list
         appending b into list
         nconcing c into list
         finally (return list))
   '(0 1 2 3 4 5 6 7 8))))
(define-test sacla-must-loop.431 (:tag :sacla)
 (assert-true
  (equal
   (loop for a in '(0 3 6)
         for b in '(1 4 7)
         for c in (copy-tree '((2) (5) (8)))
         collect a
         collect b
         nconc c)
   '(0 1 2 3 4 5 6 7 8))))
(define-test sacla-must-loop.432 (:tag :sacla)
 (assert-true (= 60 (loop for a upto 10 summing a when (oddp a) counting it))))
(define-test sacla-must-loop.433 (:tag :sacla)
 (assert-true (= 220 (loop for a upto 10 for b downfrom 20 sum a summing b))))
(define-test sacla-must-loop.434 (:tag :sacla)
 (assert-true
  (= 60
     (loop for a upto 10
           summing a into
           sum
           when (oddp a) counting it into
           sum
           finally (return sum)))))
(define-test sacla-must-loop.435 (:tag :sacla)
 (assert-true
  (= 21
     (loop for a in '(a 1 b 3 c 4 5 x 2 y z)
           if (and (numberp a) a) summing it else counting 1))))
(define-test sacla-must-loop.436 (:tag :sacla)
 (assert-true (= 5 (loop for a from 3 to 5 maximizing a minimizing a))))
(define-test sacla-must-loop.437 (:tag :sacla)
 (assert-true
  (= 3 (loop for a upto 3 for b from 6 downto 3 maximize a minimize b))))
(define-test sacla-must-loop.438 (:tag :sacla)
 (assert-true
  (equal
   (loop for a in '(0 -1 1 -2 2 -3 3)
         maximize a into plus
         minimize a into minus
         finally (return (list minus plus)))
   '(-3 3))))
(define-test sacla-must-loop.439 (:tag :sacla)
 (assert-true
  (equal
   (let (val)
     (list
      (loop for a below 10
            collecting a
            summing a into
            sum
            counting a into
            count
            maximizing a into max
            minimizing a into min
            finally (setq val (list sum count max min)))
      val))
   '((0 1 2 3 4 5 6 7 8 9) (45 10 9 0)))))
(define-test sacla-must-loop.440 (:tag :sacla)
 (assert-true (eq 'ok (loop for a below 3 collecting a finally (return 'ok)))))
(define-test sacla-must-loop.441 (:tag :sacla)
 (assert-true
  (let ((flag nil))
    (and
     (equal (loop for a below 3 collecting a finally (setq flag t)) '(0 1 2))
     flag))))
(define-test sacla-must-loop.442 (:tag :sacla)
 (assert-true
  (eq 'ok (loop for a below 3 appending (list a) finally (return 'ok)))))
(define-test sacla-must-loop.443 (:tag :sacla)
 (assert-true
  (eq 'ok (loop for a below 3 nconcing (list a) finally (return 'ok)))))
(define-test sacla-must-loop.444 (:tag :sacla)
 (assert-true (= 5 (loop for a from 1 upto 10 counting (evenp a)))))
(define-test sacla-must-loop.445 (:tag :sacla)
 (assert-true (= (loop for a downfrom 10 above 0 count a) 10)))
(define-test sacla-must-loop.446 (:tag :sacla)
 (assert-true (= (loop for a downfrom 10 above 0 counting a) 10)))
(define-test sacla-must-loop.447 (:tag :sacla)
 (assert-true (null (loop for a downfrom 10 above 0 count a into x))))
(define-test sacla-must-loop.448 (:tag :sacla)
 (assert-true (null (loop for a downfrom 10 above 0 counting a into x))))
(define-test sacla-must-loop.449 (:tag :sacla)
 (assert-true
  (= (loop for a downfrom 10 above 0 count a into x finally (return x)) 10)))
(define-test sacla-must-loop.450 (:tag :sacla)
 (assert-true
  (= (loop for a downfrom 10 above 0 counting a into x finally (return x)) 10)))
(define-test sacla-must-loop.451 (:tag :sacla)
 (assert-true
  (=
   (loop for a in '(nil a nil nil b nil c d e nil nil nil nil f)
         when a count it)
   6)))
(define-test sacla-must-loop.452 (:tag :sacla)
 (assert-true
  (=
   (loop for a in '(nil a nil nil b nil c d e nil nil nil nil f)
         when a counting it)
   6)))
(define-test sacla-must-loop.453 (:tag :sacla)
 (assert-true
  (null
   (loop for a in '(nil a nil nil b nil c d e nil nil nil nil f)
         when a count it into x))))
(define-test sacla-must-loop.454 (:tag :sacla)
 (assert-true
  (null
   (loop for a in '(nil a nil nil b nil c d e nil nil nil nil f)
         when a counting it into x))))
(define-test sacla-must-loop.455 (:tag :sacla)
 (assert-true
  (=
   (loop for a in '(nil a nil nil b nil c d e nil nil nil nil f)
         when a count it into x
         finally (return x))
   6)))
(define-test sacla-must-loop.456 (:tag :sacla)
 (assert-true
  (=
   (loop for a in '(nil a nil nil b nil c d e nil nil nil nil f)
         when a counting it into x
         finally (return x))
   6)))
(define-test sacla-must-loop.457 (:tag :sacla)
 (assert-true (= 5 (loop for i in '(a b nil c nil d e) count i))))
(define-test sacla-must-loop.458 (:tag :sacla)
 (assert-true (= (loop for a to 10 sum a) 55)))
(define-test sacla-must-loop.459 (:tag :sacla)
 (assert-true (= (loop for a to 10 summing a) 55)))
(define-test sacla-must-loop.460 (:tag :sacla)
 (assert-true
  (= (loop for a in '(0 nil 1 nil 2 3 nil 4 5 6 7 nil 8 9 10 nil) if a sum it)
     55)))
(define-test sacla-must-loop.461 (:tag :sacla)
 (assert-true
  (=
   (loop for a in '(0 nil 1 nil 2 3 nil 4 5 6 7 nil 8 9 10 nil)
         if a summing it)
   55)))
(define-test sacla-must-loop.462 (:tag :sacla)
 (assert-true
  (loop for a to 10
        sum a into
        sum
        if (oddp a) sum a into odd else sum a into even
        finally (return (= sum (+ odd even))))))
(define-test sacla-must-loop.463 (:tag :sacla)
 (assert-true
  (loop for a to 10
        summing a into
        sum
        if (oddp a) sum a into odd else summing a into even
        finally (return (= sum (+ odd even))))))
(define-test sacla-must-loop.464 (:tag :sacla)
 (assert-true (= 15 (loop for a downfrom 5 to 1 summing a))))
(define-test sacla-must-loop.465 (:tag :sacla)
 (assert-true (null (loop for a downfrom 5 to 1 summing a into n))))
(define-test sacla-must-loop.466 (:tag :sacla)
 (assert-true (= (loop for i from 1 to 4 sum i fixnum count t fixnum) 14)))
(define-test sacla-must-loop.467 (:tag :sacla)
 (assert-true (= 5 (loop for i in '(2 1 5 3 4) maximize i))))
(define-test sacla-must-loop.468 (:tag :sacla)
 (assert-true (= (loop for a in '(0 5 9) maximize a) 9)))
(define-test sacla-must-loop.469 (:tag :sacla)
 (assert-true (= (loop for a in '(0 5 9) maximizing a) 9)))
(define-test sacla-must-loop.470 (:tag :sacla)
 (assert-true (= (loop for a in '(0 9 5) maximize a) 9)))
(define-test sacla-must-loop.471 (:tag :sacla)
 (assert-true (= (loop for a in '(0 9 5) maximizing a) 9)))
(define-test sacla-must-loop.472 (:tag :sacla)
 (assert-true (= (loop for a in '(9 0 5) maximize a) 9)))
(define-test sacla-must-loop.473 (:tag :sacla)
 (assert-true (= (loop for a in '(9 0 5) maximizing a) 9)))
(define-test sacla-must-loop.474 (:tag :sacla)
 (assert-true (= (loop for a in '(9 0 9 5) maximize a) 9)))
(define-test sacla-must-loop.475 (:tag :sacla)
 (assert-true (= (loop for a in '(9 0 9 5) maximizing a) 9)))
(define-test sacla-must-loop.476 (:tag :sacla)
 (assert-true
  (let (list)
    (loop
       (when (= (first (push (random 10) list)) 9)
         (return)))
    (= (loop for a in list maximize a) 9))))
(define-test sacla-must-loop.477 (:tag :sacla)
 (assert-true
  (let (list)
    (loop
       (when (= (first (push (random 10) list)) 9)
         (return)))
    (= (loop for a in list maximizing a) 9))))
(define-test sacla-must-loop.478 (:tag :sacla)
 (assert-true
  (let (list)
    (loop
       (when (= (first (push (random 100) list)) 99)
         (return)))
    (= (loop for a in list maximize a) 99))))
(define-test sacla-must-loop.479 (:tag :sacla)
 (assert-true
  (let (list)
    (loop
       (when (= (first (push (random 100) list)) 99)
         (return)))
    (= (loop for a in list maximizing a) 99))))
(define-test sacla-must-loop.480 (:tag :sacla)
 (assert-true
  (let (list)
    (loop
       (when (= (first (push (random 1000) list)) 999)
         (return)))
    (= (loop for a in list maximize a) 999))))
(define-test sacla-must-loop.481 (:tag :sacla)
 (assert-true
  (let (list)
    (loop
       (when (= (first (push (random 1000) list)) 999)
         (return)))
    (= (loop for a in list maximizing a) 999))))
(define-test sacla-must-loop.482 (:tag :sacla)
 (assert-true (null (loop for a in '(0 5 9) maximize a into max))))
(define-test sacla-must-loop.483 (:tag :sacla)
 (assert-true (null (loop for a in '(0 5 9) maximizing a into max))))
(define-test sacla-must-loop.484 (:tag :sacla)
 (assert-true
  (= (loop for a in '(0 5 9) maximize a into max finally (return max)) 9)))
(define-test sacla-must-loop.485 (:tag :sacla)
 (assert-true
  (= (loop for a in '(0 5 9) maximizing a into max finally (return max)) 9)))
(define-test sacla-must-loop.486 (:tag :sacla)
 (assert-true
  (=
   (loop for a in '(0 5 9)
         maximize a into max of-type integer
         finally (return max))
   9)))
(define-test sacla-must-loop.487 (:tag :sacla)
 (assert-true
  (=
   (loop for a in '(0 5 9)
         maximizing a into max of-type integer
         finally (return max))
   9)))
(define-test sacla-must-loop.488 (:tag :sacla)
 (assert-true
  (=
   (loop for a in '(0.0 5.0 9.0)
         maximize a into max float
         finally (return max))
   9.0)))
(define-test sacla-must-loop.489 (:tag :sacla)
 (assert-true
  (=
   (loop for a in '(0.0 5.0 9.0)
         maximizing a into max float
         finally (return max))
   9.0)))
(define-test sacla-must-loop.490 (:tag :sacla)
 (assert-true
  (let ((series '(1.2 4.3 5.7)))
    (= 6 (loop for v in series maximize (round v) of-type fixnum)))))
(define-test sacla-must-loop.491 (:tag :sacla)
 (assert-true (= 1 (loop for i in '(2 1 5 3 4) minimize i))))
(define-test sacla-must-loop.492 (:tag :sacla)
 (assert-true (= (loop for a in '(0 5 9) minimize a) 0)))
(define-test sacla-must-loop.493 (:tag :sacla)
 (assert-true (= (loop for a in '(0 5 9) minimizing a) 0)))
(define-test sacla-must-loop.494 (:tag :sacla)
 (assert-true (= (loop for a in '(9 5 0) minimize a) 0)))
(define-test sacla-must-loop.495 (:tag :sacla)
 (assert-true (= (loop for a in '(9 5 0) minimizing a) 0)))
(define-test sacla-must-loop.496 (:tag :sacla)
 (assert-true (= (loop for a in '(9 0 5) minimize a) 0)))
(define-test sacla-must-loop.497 (:tag :sacla)
 (assert-true (= (loop for a in '(9 0 5) minimizing a) 0)))
(define-test sacla-must-loop.498 (:tag :sacla)
 (assert-true (= (loop for a in '(9 0 9 0 5 0) minimizing a) 0)))
(define-test sacla-must-loop.499 (:tag :sacla)
 (assert-true (= (loop for a in '(9 0 9 0 5 0) minimizing a) 0)))
(define-test sacla-must-loop.500 (:tag :sacla)
 (assert-true (= (loop for a in '(1 5 9) minimize a) 1)))
(define-test sacla-must-loop.501 (:tag :sacla)
 (assert-true (= (loop for a in '(1 5 9) minimizing a) 1)))
(define-test sacla-must-loop.502 (:tag :sacla)
 (assert-true (= (loop for a in '(9 5 1) minimize a) 1)))
(define-test sacla-must-loop.503 (:tag :sacla)
 (assert-true (= (loop for a in '(9 5 1) minimizing a) 1)))
(define-test sacla-must-loop.504 (:tag :sacla)
 (assert-true (= (loop for a in '(9 1 5) minimize a) 1)))
(define-test sacla-must-loop.505 (:tag :sacla)
 (assert-true (= (loop for a in '(9 1 5) minimizing a) 1)))
(define-test sacla-must-loop.506 (:tag :sacla)
 (assert-true (= (loop for a in '(9 1 9 1 5 1) minimizing a) 1)))
(define-test sacla-must-loop.507 (:tag :sacla)
 (assert-true (= (loop for a in '(9 1 9 1 5 1) minimizing a) 1)))
(define-test sacla-must-loop.508 (:tag :sacla)
 (assert-true
  (let (list)
    (loop
       (when (zerop (first (push (random 10) list)))
         (return)))
    (zerop (loop for a in list minimize a)))))
(define-test sacla-must-loop.509 (:tag :sacla)
 (assert-true
  (let (list)
    (loop
       (when (zerop (first (push (random 10) list)))
         (return)))
    (zerop (loop for a in list minimizing a)))))
(define-test sacla-must-loop.510 (:tag :sacla)
 (assert-true
  (let (list)
    (loop
       (when (zerop (first (push (random 100) list)))
         (return)))
    (zerop (loop for a in list minimize a)))))
(define-test sacla-must-loop.511 (:tag :sacla)
 (assert-true
  (let (list)
    (loop
       (when (zerop (first (push (random 100) list)))
         (return)))
    (zerop (loop for a in list minimizing a)))))
(define-test sacla-must-loop.512 (:tag :sacla)
 (assert-true
  (let (list)
    (loop
       (when (zerop (first (push (random 1000) list)))
         (return)))
    (zerop (loop for a in list minimize a)))))
(define-test sacla-must-loop.513 (:tag :sacla)
 (assert-true
  (let (list)
    (loop
       (when (zerop (first (push (random 1000) list)))
         (return)))
    (zerop (loop for a in list minimizing a)))))
(define-test sacla-must-loop.514 (:tag :sacla)
 (assert-true (null (loop for a in '(0 5 9) minimize a into min))))
(define-test sacla-must-loop.515 (:tag :sacla)
 (assert-true (null (loop for a in '(0 5 9) minimizing a into min))))
(define-test sacla-must-loop.516 (:tag :sacla)
 (assert-true
  (zerop (loop for a in '(0 5 9) minimize a into min finally (return min)))))
(define-test sacla-must-loop.517 (:tag :sacla)
 (assert-true
  (zerop (loop for a in '(0 5 9) minimizing a into min finally (return min)))))
(define-test sacla-must-loop.518 (:tag :sacla)
 (assert-true
  (zerop
   (loop for a in '(0 5 9)
         minimize a into min of-type integer
         finally (return min)))))
(define-test sacla-must-loop.519 (:tag :sacla)
 (assert-true
  (zerop
   (loop for a in '(0 5 9)
         minimizing a into min of-type integer
         finally (return min)))))
(define-test sacla-must-loop.520 (:tag :sacla)
 (assert-true
  (=
   (loop for a in '(0.0 5.0 9.0)
         minimize a into min float
         finally (return min))
   0.0)))
(define-test sacla-must-loop.521 (:tag :sacla)
 (assert-true
  (=
   (loop for a in '(0.0 5.0 9.0)
         minimizing a into min float
         finally (return min))
   0.0)))
(define-test sacla-must-loop.522 (:tag :sacla)
 (assert-true
  (= 1
     (let ((series '(1.2 4.3 5.7)))
       (loop for v of-type float in series
             minimize (round v) into result of-type fixnum
             finally (return result))))))
(define-test sacla-must-loop.523 (:tag :sacla)
 (assert-true
  (= 10 (loop for a in '(nil 1 nil 2 nil 3 nil 4) when a summing it fixnum))))
(define-test sacla-must-loop.524 (:tag :sacla)
 (assert-true
  (= 10
     (loop for a in '(nil 1 nil 2 nil 3 nil 4)
           when a summing it of-type fixnum))))
(define-test sacla-must-loop.525 (:tag :sacla)
 (assert-true
  (= 10.0
     (loop for a in '(nil 1.0 nil 2.0 nil 3.0 nil 4.0)
           when a summing it float))))
(define-test sacla-must-loop.526 (:tag :sacla)
 (assert-true
  (= 10.0
     (loop for a in '(nil 1.0 nil 2.0 nil 3.0 nil 4.0)
           when a summing it of-type float))))
(define-test sacla-must-loop.527 (:tag :sacla)
 (assert-true
  (= 10
     (loop for a in '(nil 1 nil 2 nil 3 nil 4)
           when a summing it of-type number))))
(define-test sacla-must-loop.528 (:tag :sacla)
 (assert-true
  (= 10
     (loop for a in '(nil 1 nil 2 nil 3 nil 4)
           when a summing it of-type (integer 0)))))
(define-test sacla-must-loop.529 (:tag :sacla)
 (assert-true
  (= 10 (loop for a in '(nil 1 nil 2 nil 3 nil 4) when a summing a fixnum))))
(define-test sacla-must-loop.530 (:tag :sacla)
 (assert-true
  (= 10
     (loop for a in '(nil 1 nil 2 nil 3 nil 4)
           when a summing a of-type fixnum))))
(define-test sacla-must-loop.531 (:tag :sacla)
 (assert-true
  (= 10.0
     (loop for a in '(nil 1.0 nil 2.0 nil 3.0 nil 4.0)
           when a summing a float))))
(define-test sacla-must-loop.532 (:tag :sacla)
 (assert-true
  (= 10.0
     (loop for a in '(nil 1.0 nil 2.0 nil 3.0 nil 4.0)
           when a summing a of-type float))))
(define-test sacla-must-loop.533 (:tag :sacla)
 (assert-true
  (= 10
     (loop for a in '(nil 1 nil 2 nil 3 nil 4)
           when a summing a of-type number))))
(define-test sacla-must-loop.534 (:tag :sacla)
 (assert-true
  (= 10
     (loop for a in '(nil 1 nil 2 nil 3 nil 4)
           when a summing a of-type (integer 0)))))
(define-test sacla-must-loop.535 (:tag :sacla)
 (assert-true
  (= 10
     (loop for a in '(nil 1 nil 2 nil 3 nil 4)
           when a summing a into
           sum fixnum
           finally (return sum)))))
(define-test sacla-must-loop.536 (:tag :sacla)
 (assert-true
  (= 10
     (loop for a in '(nil 1 nil 2 nil 3 nil 4)
           when a summing a into
           sum of-type fixnum
           finally (return sum)))))
(define-test sacla-must-loop.537 (:tag :sacla)
 (assert-true
  (= 10.0
     (loop for a in '(nil 1.0 nil 2.0 nil 3.0 nil 4.0)
           when a summing a into
           sum float
           finally (return sum)))))
(define-test sacla-must-loop.538 (:tag :sacla)
 (assert-true
  (= 10.0
     (loop for a in '(nil 1.0 nil 2.0 nil 3.0 nil 4.0)
           when a summing a into
           sum of-type float
           finally (return sum)))))
(define-test sacla-must-loop.539 (:tag :sacla)
 (assert-true
  (= 10
     (loop for a in '(nil 1 nil 2 nil 3 nil 4)
           when a summing a into
           sum of-type number
           finally (return sum)))))
(define-test sacla-must-loop.540 (:tag :sacla)
 (assert-true
  (= 10
     (loop for a in '(nil 1 nil 2 nil 3 nil 4)
           when a summing a into
           sum of-type (integer 0)
           finally (return sum)))))
(define-test sacla-must-loop.541 (:tag :sacla)
 (assert-true
  (= 10 (loop for a in '(nil 1 nil 2 nil 3 nil 4) when a sum it fixnum))))
(define-test sacla-must-loop.542 (:tag :sacla)
 (assert-true
  (= 10
     (loop for a in '(nil 1 nil 2 nil 3 nil 4) when a sum it of-type fixnum))))
(define-test sacla-must-loop.543 (:tag :sacla)
 (assert-true
  (= 10.0
     (loop for a in '(nil 1.0 nil 2.0 nil 3.0 nil 4.0) when a sum it float))))
(define-test sacla-must-loop.544 (:tag :sacla)
 (assert-true
  (= 10.0
     (loop for a in '(nil 1.0 nil 2.0 nil 3.0 nil 4.0)
           when a sum it of-type float))))
(define-test sacla-must-loop.545 (:tag :sacla)
 (assert-true
  (= 10
     (loop for a in '(nil 1 nil 2 nil 3 nil 4) when a sum it of-type number))))
(define-test sacla-must-loop.546 (:tag :sacla)
 (assert-true
  (= 10
     (loop for a in '(nil 1 nil 2 nil 3 nil 4)
           when a sum it of-type (integer 0)))))
(define-test sacla-must-loop.547 (:tag :sacla)
 (assert-true
  (= 10 (loop for a in '(nil 1 nil 2 nil 3 nil 4) when a sum a fixnum))))
(define-test sacla-must-loop.548 (:tag :sacla)
 (assert-true
  (= 10
     (loop for a in '(nil 1 nil 2 nil 3 nil 4) when a sum a of-type fixnum))))
(define-test sacla-must-loop.549 (:tag :sacla)
 (assert-true
  (= 10.0
     (loop for a in '(nil 1.0 nil 2.0 nil 3.0 nil 4.0) when a sum a float))))
(define-test sacla-must-loop.550 (:tag :sacla)
 (assert-true
  (= 10.0
     (loop for a in '(nil 1.0 nil 2.0 nil 3.0 nil 4.0)
           when a sum a of-type float))))
(define-test sacla-must-loop.551 (:tag :sacla)
 (assert-true
  (= 10
     (loop for a in '(nil 1 nil 2 nil 3 nil 4) when a sum a of-type number))))
(define-test sacla-must-loop.552 (:tag :sacla)
 (assert-true
  (= 10
     (loop for a in '(nil 1 nil 2 nil 3 nil 4)
           when a sum a of-type (integer 0)))))
(define-test sacla-must-loop.553 (:tag :sacla)
 (assert-true
  (= 10
     (loop for a in '(nil 1 nil 2 nil 3 nil 4)
           when a sum a into
           sum fixnum
           finally (return sum)))))
(define-test sacla-must-loop.554 (:tag :sacla)
 (assert-true
  (= 10
     (loop for a in '(nil 1 nil 2 nil 3 nil 4)
           when a sum a into
           sum of-type fixnum
           finally (return sum)))))
(define-test sacla-must-loop.555 (:tag :sacla)
 (assert-true
  (= 10.0
     (loop for a in '(nil 1.0 nil 2.0 nil 3.0 nil 4.0)
           when a sum a into
           sum float
           finally (return sum)))))
(define-test sacla-must-loop.556 (:tag :sacla)
 (assert-true
  (= 10.0
     (loop for a in '(nil 1.0 nil 2.0 nil 3.0 nil 4.0)
           when a sum a into
           sum of-type float
           finally (return sum)))))
(define-test sacla-must-loop.557 (:tag :sacla)
 (assert-true
  (= 10
     (loop for a in '(nil 1 nil 2 nil 3 nil 4)
           when a sum a into
           sum of-type number
           finally (return sum)))))
(define-test sacla-must-loop.558 (:tag :sacla)
 (assert-true
  (= 10
     (loop for a in '(nil 1 nil 2 nil 3 nil 4)
           when a sum a into
           sum of-type (integer 0)
           finally (return sum)))))
(define-test sacla-must-loop.559 (:tag :sacla)
 (assert-true
  (= 7
     (loop for a in '(nil a nil b nil c nil d e nil f g nil nil nil nil)
           counting a fixnum))))
(define-test sacla-must-loop.560 (:tag :sacla)
 (assert-true
  (= 7
     (loop for a in '(nil a nil b nil c nil d e nil f g nil nil nil nil)
           counting a of-type fixnum))))
(define-test sacla-must-loop.561 (:tag :sacla)
 (assert-true
  (= 7
     (loop for a in '(nil a nil b nil c nil d e nil f g nil nil nil nil)
           counting a of-type integer))))
(define-test sacla-must-loop.562 (:tag :sacla)
 (assert-true
  (= 7
     (loop for a in '(nil a nil b nil c nil d e nil f g nil nil nil nil)
           counting a of-type (integer 0)))))
(define-test sacla-must-loop.563 (:tag :sacla)
 (assert-true
  (= 7
     (loop for a in '(nil a nil b nil c nil d e nil f g nil nil nil nil)
           counting a of-type number))))
(define-test sacla-must-loop.564 (:tag :sacla)
 (assert-true
  (= 7
     (loop for a in '(nil a nil b nil c nil d e nil f g nil nil nil nil)
           counting a into x fixnum
           finally (return x)))))
(define-test sacla-must-loop.565 (:tag :sacla)
 (assert-true
  (= 7
     (loop for a in '(nil a nil b nil c nil d e nil f g nil nil nil nil)
           counting a into x of-type fixnum
           finally (return x)))))
(define-test sacla-must-loop.566 (:tag :sacla)
 (assert-true
  (= 7
     (loop for a in '(nil a nil b nil c nil d e nil f g nil nil nil nil)
           counting a into x of-type integer
           finally (return x)))))
(define-test sacla-must-loop.567 (:tag :sacla)
 (assert-true
  (= 7
     (loop for a in '(nil a nil b nil c nil d e nil f g nil nil nil nil)
           counting a into x of-type (integer 0)
           finally (return x)))))
(define-test sacla-must-loop.568 (:tag :sacla)
 (assert-true
  (= 7
     (loop for a in '(nil a nil b nil c nil d e nil f g nil nil nil nil)
           counting a into x of-type number
           finally (return x)))))
(define-test sacla-must-loop.569 (:tag :sacla)
 (assert-true (= 99 (loop for a in '(3 5 8 0 7 7 99 3) maximize a fixnum))))
(define-test sacla-must-loop.570 (:tag :sacla)
 (assert-true
  (= 99 (loop for a in '(3 5 8 0 7 7 99 3) maximize a of-type fixnum))))
(define-test sacla-must-loop.571 (:tag :sacla)
 (assert-true
  (= 99.0
     (loop for a in '(3.0 5.0 8.0 0.0 7.0 7.0 99.0 3.0) maximize a float))))
(define-test sacla-must-loop.572 (:tag :sacla)
 (assert-true
  (= 99.0
     (loop for a in '(3.0 5.0 8.0 0.0 7.0 7.0 99.0 3.0)
           maximize a of-type float))))
(define-test sacla-must-loop.573 (:tag :sacla)
 (assert-true
  (= 99.0
     (loop for a in '(3.0 5.0 2.2 8.0 0 3/5 7.0 7 99 3.0)
           maximize a of-type real))))
(define-test sacla-must-loop.574 (:tag :sacla)
 (assert-true
  (= 99 (loop for a in '(3 5 8 0 7 7 99 3) maximize a of-type (integer 0)))))
(define-test sacla-must-loop.575 (:tag :sacla)
 (assert-true
  (= 99
     (loop for a in '(3 5 8 0 7 7 99 3)
           maximize a into max fixnum
           finally (return max)))))
(define-test sacla-must-loop.576 (:tag :sacla)
 (assert-true
  (= 99
     (loop for a in '(3 5 8 0 7 7 99 3)
           maximize a into max of-type fixnum
           finally (return max)))))
(define-test sacla-must-loop.577 (:tag :sacla)
 (assert-true
  (= 99.0
     (loop for a in '(3.0 5.0 8.0 0.0 7.0 7.0 99.0 3.0)
           maximize a into max float
           finally (return max)))))
(define-test sacla-must-loop.578 (:tag :sacla)
 (assert-true
  (= 99.0
     (loop for a in '(3.0 5.0 8.0 0.0 7.0 7.0 99.0 3.0)
           maximize a into max of-type float
           finally (return max)))))
(define-test sacla-must-loop.579 (:tag :sacla)
 (assert-true
  (= 99.0
     (loop for a in '(3.0 5.0 2.2 8.0 0 3/5 7.0 7 99 3.0)
           maximize a into max of-type real
           finally (return max)))))
(define-test sacla-must-loop.580 (:tag :sacla)
 (assert-true
  (= 99
     (loop for a in '(3 5 8 0 7 7 99 3)
           maximize a into max of-type (integer 0)
           finally (return max)))))
(define-test sacla-must-loop.581 (:tag :sacla)
 (assert-true
  (= 99
     (loop for a in '(3 nil 5 8 nil 0 nil 7 7 99 3)
           when a maximize it fixnum))))
(define-test sacla-must-loop.582 (:tag :sacla)
 (assert-true
  (= 99
     (loop for a in '(nil 3 nil 5 nil 8 0 7 7 nil 99 nil 3)
           when a maximize it of-type fixnum))))
(define-test sacla-must-loop.583 (:tag :sacla)
 (assert-true
  (= 99.0
     (loop for a in
               '(3.0 nil 5.0 8.0 0.0 nil nil nil nil 7.0 nil 7.0 99.0 nil 3.0
                 nil nil nil)
           when a maximize it float))))
(define-test sacla-must-loop.584 (:tag :sacla)
 (assert-true
  (= 99.0
     (loop for a in
               '(nil nil nil nil nil 3.0 nil 5.0 8.0 0.0 nil nil nil 7.0 7.0
                 nil nil 99.0 3.0)
           when a maximize it of-type float))))
(define-test sacla-must-loop.585 (:tag :sacla)
 (assert-true
  (= 99.0
     (loop for a in
               '(3.0 5.0 nil nil 2.2 nil nil 8.0 0 nil nil 3/5 nil nil 7.0 7 99
                 3.0)
           when a maximize it of-type real))))
(define-test sacla-must-loop.586 (:tag :sacla)
 (assert-true
  (= 99
     (loop for a in '(3 nil nil 5 8 0 nil nil 7 7 99 nil nil 3)
           when a maximize a of-type (integer 0)))))
(define-test sacla-must-loop.587 (:tag :sacla)
 (assert-true
  (= 99
     (loop for a in '(3 nil 5 8 nil 0 nil 7 7 99 3)
           when a maximize it into max fixnum
           finally (return max)))))
(define-test sacla-must-loop.588 (:tag :sacla)
 (assert-true
  (= 99
     (loop for a in '(nil 3 nil 5 nil 8 0 7 7 nil 99 nil 3)
           when a maximize it into max of-type fixnum
           finally (return max)))))
(define-test sacla-must-loop.589 (:tag :sacla)
 (assert-true
  (= 99.0
     (loop for a in
               '(3.0 nil 5.0 8.0 0.0 nil nil nil nil 7.0 nil 7.0 99.0 nil 3.0
                 nil nil nil)
           when a maximize it into max float
           finally (return max)))))
(define-test sacla-must-loop.590 (:tag :sacla)
 (assert-true
  (= 99.0
     (loop for a in
               '(nil nil nil nil nil 3.0 nil 5.0 8.0 0.0 nil nil nil 7.0 7.0
                 nil nil 99.0 3.0)
           when a maximize it into max of-type float
           finally (return max)))))
(define-test sacla-must-loop.591 (:tag :sacla)
 (assert-true
  (= 99.0
     (loop for a in
               '(3.0 5.0 nil nil 2.2 nil nil 8.0 0 nil nil 3/5 nil nil 7.0 7 99
                 3.0)
           when a maximize it into max of-type real
           finally (return max)))))
(define-test sacla-must-loop.592 (:tag :sacla)
 (assert-true
  (= 99
     (loop for a in '(3 nil nil 5 8 0 nil nil 7 7 99 nil nil 3)
           when a maximize it into max of-type (integer 0)
           finally (return max)))))
(define-test sacla-must-loop.593 (:tag :sacla)
 (assert-true (= 99 (loop for a in '(3 5 8 0 7 7 99 3) maximizing a fixnum))))
(define-test sacla-must-loop.594 (:tag :sacla)
 (assert-true
  (= 99 (loop for a in '(3 5 8 0 7 7 99 3) maximizing a of-type fixnum))))
(define-test sacla-must-loop.595 (:tag :sacla)
 (assert-true
  (= 99.0
     (loop for a in '(3.0 5.0 8.0 0.0 7.0 7.0 99.0 3.0) maximizing a float))))
(define-test sacla-must-loop.596 (:tag :sacla)
 (assert-true
  (= 99.0
     (loop for a in '(3.0 5.0 8.0 0.0 7.0 7.0 99.0 3.0)
           maximizing a of-type float))))
(define-test sacla-must-loop.597 (:tag :sacla)
 (assert-true
  (= 99.0
     (loop for a in '(3.0 5.0 2.2 8.0 0 3/5 7.0 7 99 3.0)
           maximizing a of-type real))))
(define-test sacla-must-loop.598 (:tag :sacla)
 (assert-true
  (= 99 (loop for a in '(3 5 8 0 7 7 99 3) maximizing a of-type (integer 0)))))
(define-test sacla-must-loop.599 (:tag :sacla)
 (assert-true
  (= 99
     (loop for a in '(3 5 8 0 7 7 99 3)
           maximizing a into max fixnum
           finally (return max)))))
(define-test sacla-must-loop.600 (:tag :sacla)
 (assert-true
  (= 99
     (loop for a in '(3 5 8 0 7 7 99 3)
           maximizing a into max of-type fixnum
           finally (return max)))))
(define-test sacla-must-loop.601 (:tag :sacla)
 (assert-true
  (= 99.0
     (loop for a in '(3.0 5.0 8.0 0.0 7.0 7.0 99.0 3.0)
           maximizing a into max float
           finally (return max)))))
(define-test sacla-must-loop.602 (:tag :sacla)
 (assert-true
  (= 99.0
     (loop for a in '(3.0 5.0 8.0 0.0 7.0 7.0 99.0 3.0)
           maximizing a into max of-type float
           finally (return max)))))
(define-test sacla-must-loop.603 (:tag :sacla)
 (assert-true
  (= 99.0
     (loop for a in '(3.0 5.0 2.2 8.0 0 3/5 7.0 7 99 3.0)
           maximizing a into max of-type real
           finally (return max)))))
(define-test sacla-must-loop.604 (:tag :sacla)
 (assert-true
  (= 99
     (loop for a in '(3 5 8 0 7 7 99 3)
           maximizing a into max of-type (integer 0)
           finally (return max)))))
(define-test sacla-must-loop.605 (:tag :sacla)
 (assert-true
  (= 99
     (loop for a in '(3 nil 5 8 nil 0 nil 7 7 99 3)
           when a maximizing it fixnum))))
(define-test sacla-must-loop.606 (:tag :sacla)
 (assert-true
  (= 99
     (loop for a in '(nil 3 nil 5 nil 8 0 7 7 nil 99 nil 3)
           when a maximizing it of-type fixnum))))
(define-test sacla-must-loop.607 (:tag :sacla)
 (assert-true
  (= 99.0
     (loop for a in
               '(3.0 nil 5.0 8.0 0.0 nil nil nil nil 7.0 nil 7.0 99.0 nil 3.0
                 nil nil nil)
           when a maximizing it float))))
(define-test sacla-must-loop.608 (:tag :sacla)
 (assert-true
  (= 99.0
     (loop for a in
               '(nil nil nil nil nil 3.0 nil 5.0 8.0 0.0 nil nil nil 7.0 7.0
                 nil nil 99.0 3.0)
           when a maximizing it of-type float))))
(define-test sacla-must-loop.609 (:tag :sacla)
 (assert-true
  (= 99.0
     (loop for a in
               '(3.0 5.0 nil nil 2.2 nil nil 8.0 0 nil nil 3/5 nil nil 7.0 7 99
                 3.0)
           when a maximizing it of-type real))))
(define-test sacla-must-loop.610 (:tag :sacla)
 (assert-true
  (= 99
     (loop for a in '(3 nil nil 5 8 0 nil nil 7 7 99 nil nil 3)
           when a maximizing a of-type (integer 0)))))
(define-test sacla-must-loop.611 (:tag :sacla)
 (assert-true
  (= 99
     (loop for a in '(3 nil 5 8 nil 0 nil 7 7 99 3)
           when a maximizing it into max fixnum
           finally (return max)))))
(define-test sacla-must-loop.612 (:tag :sacla)
 (assert-true
  (= 99
     (loop for a in '(nil 3 nil 5 nil 8 0 7 7 nil 99 nil 3)
           when a maximizing it into max of-type fixnum
           finally (return max)))))
(define-test sacla-must-loop.613 (:tag :sacla)
 (assert-true
  (= 99.0
     (loop for a in
               '(3.0 nil 5.0 8.0 0.0 nil nil nil nil 7.0 nil 7.0 99.0 nil 3.0
                 nil nil nil)
           when a maximizing it into max float
           finally (return max)))))
(define-test sacla-must-loop.614 (:tag :sacla)
 (assert-true
  (= 99.0
     (loop for a in
               '(nil nil nil nil nil 3.0 nil 5.0 8.0 0.0 nil nil nil 7.0 7.0
                 nil nil 99.0 3.0)
           when a maximizing it into max of-type float
           finally (return max)))))
(define-test sacla-must-loop.615 (:tag :sacla)
 (assert-true
  (= 99.0
     (loop for a in
               '(3.0 5.0 nil nil 2.2 nil nil 8.0 0 nil nil 3/5 nil nil 7.0 7 99
                 3.0)
           when a maximizing it into max of-type real
           finally (return max)))))
(define-test sacla-must-loop.616 (:tag :sacla)
 (assert-true
  (= 99
     (loop for a in '(3 nil nil 5 8 0 nil nil 7 7 99 nil nil 3)
           when a maximizing it into max of-type (integer 0)
           finally (return max)))))
(define-test sacla-must-loop.617 (:tag :sacla)
 (assert-true (= 3 (loop for a in '(3 5 8 4 7 7 99 3) minimize a fixnum))))
(define-test sacla-must-loop.618 (:tag :sacla)
 (assert-true
  (= 3 (loop for a in '(3 5 8 4 7 7 99 3) minimize a of-type fixnum))))
(define-test sacla-must-loop.619 (:tag :sacla)
 (assert-true
  (= 3.0 (loop for a in '(5.0 8.0 7.0 3.0 7.0 99.0) minimize a float))))
(define-test sacla-must-loop.620 (:tag :sacla)
 (assert-true
  (= 3.0 (loop for a in '(5.0 8.0 7.0 3.0 7.0 99.0) minimize a of-type float))))
(define-test sacla-must-loop.621 (:tag :sacla)
 (assert-true
  (= 3.0
     (loop for a in '(5.0 8 7 3 7.0 3.0 99.0 1000) minimize a of-type real))))
(define-test sacla-must-loop.622 (:tag :sacla)
 (assert-true
  (= 5 (loop for a in '(6 5 8 7 7 99) minimize a of-type (integer 0)))))
(define-test sacla-must-loop.623 (:tag :sacla)
 (assert-true
  (= 3
     (loop for a in '(5 8 4 7 7 99 3)
           minimize a into min fixnum
           finally (return min)))))
(define-test sacla-must-loop.624 (:tag :sacla)
 (assert-true
  (= 3
     (loop for a in '(5 8 4 7 7 99 3)
           minimize a into min of-type fixnum
           finally (return min)))))
(define-test sacla-must-loop.625 (:tag :sacla)
 (assert-true
  (= 3.0
     (loop for a in '(5.0 8.0 4.0 7.0 7.0 99.0 3.0)
           minimize a into min float
           finally (return min)))))
(define-test sacla-must-loop.626 (:tag :sacla)
 (assert-true
  (= 3.0
     (loop for a in '(5.0 8.0 4.0 7.0 7.0 99.0 3.0)
           minimize a into min of-type float
           finally (return min)))))
(define-test sacla-must-loop.627 (:tag :sacla)
 (assert-true
  (= 3.0
     (loop for a in '(5.0 8 4.0 31/3 7.0 7 99.0 3.0)
           minimize a into min of-type real
           finally (return min)))))
(define-test sacla-must-loop.628 (:tag :sacla)
 (assert-true
  (= 5
     (loop for a in '(6 5 8 7 7 99)
           minimize a into min of-type (integer 0)
           finally (return min)))))
(define-test sacla-must-loop.629 (:tag :sacla)
 (assert-true
  (= 3
     (loop for a in '(nil 5 8 nil nil 7 7 nil 99 3)
           when a minimize it fixnum))))
(define-test sacla-must-loop.630 (:tag :sacla)
 (assert-true
  (= 3
     (loop for a in '(nil 5 8 nil nil 7 7 nil 99 3)
           when a minimize it of-type fixnum))))
(define-test sacla-must-loop.631 (:tag :sacla)
 (assert-true
  (= 3.0
     (loop for a in '(nil 5.0 8.0 nil nil 7.0 7.0 nil 99.0 3.0)
           when a minimize it float))))
(define-test sacla-must-loop.632 (:tag :sacla)
 (assert-true
  (= 3.0
     (loop for a in '(nil 5.0 8.0 nil nil 7.0 7.0 nil 99.0 3.0)
           when a minimize it of-type float))))
(define-test sacla-must-loop.633 (:tag :sacla)
 (assert-true
  (= 3
     (loop for a in '(nil 5.0 8.0 nil nil 7.0 7.0 nil 99.0 3.0)
           when a minimize it of-type real))))
(define-test sacla-must-loop.634 (:tag :sacla)
 (assert-true
  (= 3
     (loop for a in '(nil 5 8 nil nil 7 7 nil 99 3)
           when a minimize it of-type (integer 0)))))
(define-test sacla-must-loop.635 (:tag :sacla)
 (assert-true
  (= -99
     (loop for a in '(nil -5 8 nil nil 7 7 nil -99 3)
           when a minimize it of-type (integer)))))
(define-test sacla-must-loop.636 (:tag :sacla)
 (assert-true (= 3 (loop for a in '(3 5 8 4 7 7 99 3) minimizing a fixnum))))
(define-test sacla-must-loop.637 (:tag :sacla)
 (assert-true
  (= 3 (loop for a in '(3 5 8 4 7 7 99 3) minimizing a of-type fixnum))))
(define-test sacla-must-loop.638 (:tag :sacla)
 (assert-true
  (= 3.0 (loop for a in '(5.0 8.0 7.0 3.0 7.0 99.0) minimizing a float))))
(define-test sacla-must-loop.639 (:tag :sacla)
 (assert-true
  (= 3.0
     (loop for a in '(5.0 8.0 7.0 3.0 7.0 99.0) minimizing a of-type float))))
(define-test sacla-must-loop.640 (:tag :sacla)
 (assert-true
  (= 3.0
     (loop for a in '(5.0 8 7 3 7.0 3.0 99.0 1000) minimizing a of-type real))))
(define-test sacla-must-loop.641 (:tag :sacla)
 (assert-true
  (= 5 (loop for a in '(6 5 8 7 7 99) minimizing a of-type (integer 0)))))
(define-test sacla-must-loop.642 (:tag :sacla)
 (assert-true
  (= 3
     (loop for a in '(5 8 4 7 7 99 3)
           minimizing a into min fixnum
           finally (return min)))))
(define-test sacla-must-loop.643 (:tag :sacla)
 (assert-true
  (= 3
     (loop for a in '(5 8 4 7 7 99 3)
           minimizing a into min of-type fixnum
           finally (return min)))))
(define-test sacla-must-loop.644 (:tag :sacla)
 (assert-true
  (= 3.0
     (loop for a in '(5.0 8.0 4.0 7.0 7.0 99.0 3.0)
           minimizing a into min float
           finally (return min)))))
(define-test sacla-must-loop.645 (:tag :sacla)
 (assert-true
  (= 3.0
     (loop for a in '(5.0 8.0 4.0 7.0 7.0 99.0 3.0)
           minimizing a into min of-type float
           finally (return min)))))
(define-test sacla-must-loop.646 (:tag :sacla)
 (assert-true
  (= 3.0
     (loop for a in '(5.0 8 4.0 31/3 7.0 7 99.0 3.0)
           minimizing a into min of-type real
           finally (return min)))))
(define-test sacla-must-loop.647 (:tag :sacla)
 (assert-true
  (= 5
     (loop for a in '(6 5 8 7 7 99)
           minimizing a into min of-type (integer 0)
           finally (return min)))))
(define-test sacla-must-loop.648 (:tag :sacla)
 (assert-true
  (= 3
     (loop for a in '(nil 5 8 nil nil 7 7 nil 99 3)
           when a minimizing it fixnum))))
(define-test sacla-must-loop.649 (:tag :sacla)
 (assert-true
  (= 3
     (loop for a in '(nil 5 8 nil nil 7 7 nil 99 3)
           when a minimizing it of-type fixnum))))
(define-test sacla-must-loop.650 (:tag :sacla)
 (assert-true
  (= 3.0
     (loop for a in '(nil 5.0 8.0 nil nil 7.0 7.0 nil 99.0 3.0)
           when a minimizing it float))))
(define-test sacla-must-loop.651 (:tag :sacla)
 (assert-true
  (= 3.0
     (loop for a in '(nil 5.0 8.0 nil nil 7.0 7.0 nil 99.0 3.0)
           when a minimizing it of-type float))))
(define-test sacla-must-loop.652 (:tag :sacla)
 (assert-true
  (= 3
     (loop for a in '(nil 5.0 8.0 nil nil 7.0 7.0 nil 99.0 3.0)
           when a minimizing it of-type real))))
(define-test sacla-must-loop.653 (:tag :sacla)
 (assert-true
  (= 3
     (loop for a in '(nil 5 8 nil nil 7 7 nil 99 3)
           when a minimizing it of-type (integer 0)))))
(define-test sacla-must-loop.654 (:tag :sacla)
 (assert-true
  (= -99
     (loop for a in '(nil -5 8 nil nil 7 7 nil -99 3)
           when a minimizing it of-type (integer)))))
(define-test sacla-must-loop.655 (:tag :sacla)
 (assert-true
  (eq 'ok (loop for i from 0 upto 10 summing i finally (return 'ok)))))
(define-test sacla-must-loop.656 (:tag :sacla)
 (assert-true
  (eq 'ok
      (loop for i in '(nil nil 3 nil 5 nil 6)
            counting i
            finally (return 'ok)))))
(define-test sacla-must-loop.657 (:tag :sacla)
 (assert-true
  (eq 'ok
      (loop for i in '(nil nil 3 nil 5 nil 6)
            when i maximizing it
            finally (return 'ok)))))
(define-test sacla-must-loop.658 (:tag :sacla)
 (assert-true
  (eq 'ok
      (loop for i in '(nil nil 3 nil 5 nil 6)
            when i minimizing it
            finally (return 'ok)))))
(define-test sacla-must-loop.659 (:tag :sacla)
 (assert-true (null (loop with x = '(a b c d) while x do (pop x)))))
(define-test sacla-must-loop.660 (:tag :sacla)
 (assert-true
  (equal
   (loop with stack = nil
         and x = '(0 1 2 3)
         while x
         do (push (pop x) stack)
         finally (return stack))
   '(3 2 1 0))))
(define-test sacla-must-loop.661 (:tag :sacla)
 (assert-true
  (equal
   (loop with stack = nil
         and x = '(0 1 2 3)
         until (null x)
         do (push (pop x) stack)
         finally (return stack))
   '(3 2 1 0))))
(define-test sacla-must-loop.662 (:tag :sacla)
 (assert-true
  (equal
   (let ((stack '(a b c d e f)))
     (loop for item = (length stack) then (pop stack)
           collect item
           while stack))
   '(6 a b c d e f))))
(define-test sacla-must-loop.663 (:tag :sacla)
 (assert-true
  (equal (loop for i fixnum from 3 when (oddp i) collect i while (< i 5))
         '(3 5))))
(define-test sacla-must-loop.664 (:tag :sacla)
 (assert-true
  (equal
   (loop for a below 10
         when (and (evenp a) a) collect it
         while (< a 6)
         collect a)
   '(0 0 1 2 2 3 4 4 5 6))))
(define-test sacla-must-loop.665 (:tag :sacla)
 (assert-true
  (equal
   (loop for a below 10
         when (and (evenp a) a) collect it
         until (>= a 6)
         collect a)
   '(0 0 1 2 2 3 4 4 5 6))))
(define-test sacla-must-loop.666 (:tag :sacla)
 (assert-true
  (equal
   (loop for a below 10
         when (and (evenp a) a) collect it
         while (< a 6)
         collect a
         until (>= a 4)
         collect a)
   '(0 0 0 1 1 2 2 2 3 3 4 4))))
(define-test sacla-must-loop.667 (:tag :sacla)
 (assert-true (= 3 (loop with x = 0 repeat 3 do (incf x) finally (return x)))))
(define-test sacla-must-loop.668 (:tag :sacla)
 (assert-true (= 1000 (loop repeat 1000 counting 1))))
(define-test sacla-must-loop.669 (:tag :sacla)
 (assert-true (null (loop repeat 3))))
(define-test sacla-must-loop.670 (:tag :sacla)
 (assert-true (null (loop repeat 0))))
(define-test sacla-must-loop.671 (:tag :sacla)
 (assert-true
  (let ((body-flag nil))
    (and (null (loop repeat 0 do (setq body-flag t))) (null body-flag)))))
(define-test sacla-must-loop.672 (:tag :sacla)
 (assert-true
  (= 1
     (let ((x 0))
       (loop repeat (incf x) sum x)))))
(define-test sacla-must-loop.673 (:tag :sacla)
 (assert-true
  (= 4
     (let ((x 1))
       (loop repeat (incf x) sum x)))))
(define-test sacla-must-loop.674 (:tag :sacla)
 (assert-true
  (= 9
     (let ((x 2))
       (loop repeat (incf x) sum x)))))
(define-test sacla-must-loop.675 (:tag :sacla)
 (assert-true
  (= 16
     (let ((x 3))
       (loop repeat (incf x) sum x)))))
(define-test sacla-must-loop.676 (:tag :sacla)
 (assert-true (null (loop repeat -15 return t))))
(define-test sacla-must-loop.677 (:tag :sacla)
 (assert-true
  (let ((body-flag nil))
    (and (null (loop repeat -10 do (setq body-flag t))) (null body-flag)))))
(define-test sacla-must-loop.678 (:tag :sacla)
 (assert-true
  (let ((eval-count 0) (loop-count 0))
    (loop repeat (progn
                   (incf eval-count)
                   2)
          do (incf loop-count))
    (and (= 1 eval-count) (= 2 loop-count)))))
(define-test sacla-must-loop.679 (:tag :sacla)
 (assert-true
  (let ((eval-count 0) (loop-count 0))
    (loop repeat (progn
                   (incf eval-count)
                   0)
          do (incf loop-count))
    (and (= 1 eval-count) (zerop loop-count)))))
(define-test sacla-must-loop.680 (:tag :sacla)
 (assert-true
  (let ((eval-count 0) (loop-count 0))
    (loop repeat (progn
                   (incf eval-count)
                   -100)
          do (incf loop-count))
    (and (= 1 eval-count) (zerop loop-count)))))
(define-test sacla-must-loop.681 (:tag :sacla)
 (assert-true (eq t (loop for i from 0 to 10 always (< i 11)))))
(define-test sacla-must-loop.682 (:tag :sacla)
 (assert-true (eq t (loop for a in 'nil always (oddp a)))))
(define-test sacla-must-loop.683 (:tag :sacla)
 (assert-true (null (loop for a in '(0 1 2) always (oddp a)))))
(define-test sacla-must-loop.684 (:tag :sacla)
 (assert-true (eq t (loop for a in '(1 3 5) always (oddp a)))))
(define-test sacla-must-loop.685 (:tag :sacla)
 (assert-true
  (let ((flag nil))
    (and
     (null
      (loop for i from 0 to 10
            always (< i 5)
            finally (setq flag t)
                    (return t)))
     (not flag)))))
(define-test sacla-must-loop.686 (:tag :sacla)
 (assert-true
  (eq 'ok (loop for i below 3 always (numberp i) finally (return 'ok)))))
(define-test sacla-must-loop.687 (:tag :sacla)
 (assert-true (eq t (loop repeat 3 always t))))
(define-test sacla-must-loop.688 (:tag :sacla)
 (assert-true
  (handler-case
      (macroexpand '(loop for i from 0 upto 10 always (integerp i) collect i))
    (program-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-loop.689 (:tag :sacla)
 (assert-true (eq t (loop for i from 0 to 10 never (> i 11)))))
(define-test sacla-must-loop.690 (:tag :sacla)
 (assert-true (eq t (loop for a in 'nil never (oddp a)))))
(define-test sacla-must-loop.691 (:tag :sacla)
 (assert-true (null (loop for a in '(0 1 2) never (oddp a)))))
(define-test sacla-must-loop.692 (:tag :sacla)
 (assert-true (eq t (loop for a in '(1 3 5) never (evenp a)))))
(define-test sacla-must-loop.693 (:tag :sacla)
 (assert-true (null (loop never t finally (return t)))))
(define-test sacla-must-loop.694 (:tag :sacla)
 (assert-true
  (let ((flag nil))
    (and
     (null
      (loop for a below 3
            never (oddp a)
            finally (setq flag t)
                    (return t)))
     (null flag)))))
(define-test sacla-must-loop.695 (:tag :sacla)
 (assert-true
  (eq 'ok (loop for i below 3 never (consp i) finally (return 'ok)))))
(define-test sacla-must-loop.696 (:tag :sacla)
 (assert-true (eq t (loop repeat 3 never nil))))
(define-test sacla-must-loop.697 (:tag :sacla)
 (assert-true
  (handler-case
      (macroexpand
       '(loop for i from 0 upto 10 never (integerp i) append (list i)))
    (program-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-loop.698 (:tag :sacla)
 (assert-true (null (loop for a in '(0 2 4) thereis (oddp a)))))
(define-test sacla-must-loop.699 (:tag :sacla)
 (assert-true
  (= 11
     (loop for i from 0
           thereis (when (> i 10)
                     i)))))
(define-test sacla-must-loop.700 (:tag :sacla)
 (assert-true (eq (loop thereis 'someone) 'someone)))
(define-test sacla-must-loop.701 (:tag :sacla)
 (assert-true
  (eq (loop for i from 1 to 10 thereis (> i 11) finally (return 'got-here))
      'got-here)))
(define-test sacla-must-loop.702 (:tag :sacla)
 (assert-true
  (let ((count 0))
    (and
     (null
      (loop for a below 10
            for b in '(nil nil nil nil c)
            always (< a 8)
            never b
            do (incf count)))
     (= count 4)))))
(define-test sacla-must-loop.703 (:tag :sacla)
 (assert-true
  (eq
   (loop for a in '(nil nil nil found-it! nil nil)
         for b from 10 downto 0
         never (< b 0)
         thereis a)
   'found-it!)))
(define-test sacla-must-loop.704 (:tag :sacla)
 (assert-true (= 4 (loop for i in '(1 2 3 4 5 6) thereis (and (> i 3) i)))))
(define-test sacla-must-loop.705 (:tag :sacla)
 (assert-true
  (let ((flag nil))
    (loop for a below 3 thereis (and (oddp a) a) finally (setq flag t))
    (null flag))))
(define-test sacla-must-loop.706 (:tag :sacla)
 (assert-true
  (eq 'ok (loop for i below 3 thereis (consp i) finally (return 'ok)))))
(define-test sacla-must-loop.707 (:tag :sacla)
 (assert-true (null (loop repeat 3 thereis nil))))
(define-test sacla-must-loop.708 (:tag :sacla)
 (assert-true
  (handler-case
      (macroexpand
       '(loop for i from 0 upto 10 thereis (integerp i) nconc (list i)))
    (program-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-loop.709 (:tag :sacla)
 (assert-true (loop named bar do (return-from bar t))))
(define-test sacla-must-loop.710 (:tag :sacla)
 (assert-true
  (eq t (loop named outer do (loop named inner do (return-from outer t))))))
(define-test sacla-must-loop.711 (:tag :sacla)
 (assert-true
  (equal
   (loop for (a b c) of-type (integer integer float) in
             '((1 2 4.0) (5 6 8.3) (8 9 10.4))
         collect (list c b a))
   '((4.0 2 1) (8.3 6 5) (10.4 9 8)))))
(define-test sacla-must-loop.712 (:tag :sacla)
 (assert-true
  (equal
   (loop for (a b c) of-type float in
             '((1.0 2.0 4.0) (5.0 6.0 8.3) (8.0 9.0 10.4))
         collect (list c b a))
   '((4.0 2.0 1.0) (8.3 6.0 5.0) (10.4 9.0 8.0)))))
(define-test sacla-must-loop.713 (:tag :sacla)
 (assert-true
  (equal
   (loop with (a b) of-type float = '(1.0 2.0)
         and (c d) of-type integer = '(3 4)
         and (e f)
         return (list a b c d e f))
   '(1.0 2.0 3 4 nil nil))))
(define-test sacla-must-loop.714 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for (a (b) ((c))) in '((0 (1) ((2))) (3 (4) ((5))) (6 (7) ((8))))
           do (push (list a b c) stack))
     stack)
   '((6 7 8) (3 4 5) (0 1 2)))))
(define-test sacla-must-loop.715 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for (a nil ((b))) in '((0 (1) ((2))) (3 (4) ((5))) (6 (7) ((8))))
           do (push (list a b) stack))
     stack)
   '((6 8) (3 5) (0 2)))))
(define-test sacla-must-loop.716 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for (a nil ((((b))))) in
               '((0 (1) ((((2))))) (3 (4) ((((5))))) (6 (7) ((((8))))))
           do (push (list a b) stack))
     stack)
   '((6 8) (3 5) (0 2)))))
(define-test sacla-must-loop.717 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for (a . b) in '((0 . 1) (2 . 3)) do (push (cons a b) stack))
     stack)
   '((2 . 3) (0 . 1)))))
(define-test sacla-must-loop.718 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for (a b) in '((0 1) (2 3)) do (push (list a b) stack))
     stack)
   '((2 3) (0 1)))))
(define-test sacla-must-loop.719 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for (a) on '(0 1 2 3) do (push a stack))
     stack)
   '(3 2 1 0))))
(define-test sacla-must-loop.720 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for (a . b) on '(0 1 2 3 4) do (push (list a b) stack))
     stack)
   '((4 nil) (3 (4)) (2 (3 4)) (1 (2 3 4)) (0 (1 2 3 4))))))
(define-test sacla-must-loop.721 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for (a b) across #((0 1) (2 3) (4 5)) do (push (list a b) stack))
     stack)
   '((4 5) (2 3) (0 1)))))
(define-test sacla-must-loop.722 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (loop for (a ((b))) across #((0 ((1))) (2 ((3))) (4 ((5))))
           do (push (list a b) stack))
     stack)
   '((4 5) (2 3) (0 1)))))
(define-test sacla-must-loop.723 (:tag :sacla)
 (assert-true (equal (loop with (a b) = '(0 1) return (list a b)) '(0 1))))
(define-test sacla-must-loop.724 (:tag :sacla)
 (assert-true
  (equal (loop with (a b c) = '(0) return (list a b c)) '(0 nil nil))))
(define-test sacla-must-loop.725 (:tag :sacla)
 (assert-true (= 2 (loop with (nil nil x) = '(0 1 2) return x))))
(define-test sacla-must-loop.726 (:tag :sacla)
 (assert-true
  (equal (loop for (a b c) in '((0) (1) (2)) collect (list a b c))
         '((0 nil nil) (1 nil nil) (2 nil nil)))))
(define-test sacla-must-loop.727 (:tag :sacla)
 (assert-true
  (equal (loop for (a nil b) in '((0 1 2) (1 2 3) (2 3 4)) collect (list a b))
         '((0 2) (1 3) (2 4)))))
(define-test sacla-must-loop.728 (:tag :sacla)
 (assert-true
  (equal
   (loop for (a . b) t in '((0 . x) (1 . y) (2 . z)) collecting (cons a b))
   '((0 . x) (1 . y) (2 . z)))))
(define-test sacla-must-loop.729 (:tag :sacla)
 (assert-true
  (equal
   (loop for (a . b) of-type t in '((0 . x) (1 . y) (2 . z))
         collecting (cons a b))
   '((0 . x) (1 . y) (2 . z)))))
(define-test sacla-must-loop.730 (:tag :sacla)
 (assert-true
  (equal
   (loop for (a . b) of-type (fixnum . symbol) in '((0 . x) (1 . y) (2 . z))
         collecting (cons a b))
   '((0 . x) (1 . y) (2 . z)))))
(define-test sacla-must-loop.731 (:tag :sacla)
 (assert-true
  (equal
   (loop for (a ((b))) of-type (fixnum ((symbol))) in
             '((0 ((x))) (1 ((y))) (2 ((z))))
         collecting (cons a b))
   '((0 . x) (1 . y) (2 . z)))))
(define-test sacla-must-loop.732 (:tag :sacla)
 (assert-true
  (equal
   (loop for (a ((b))) of-type (fixnum symbol) in
             '((0 ((x))) (1 ((y))) (2 ((z))))
         collecting (cons a b))
   '((0 . x) (1 . y) (2 . z)))))
(define-test sacla-must-loop.733 (:tag :sacla)
 (assert-true
  (equal
   (loop for (a ((b))) fixnum in '((0 ((10))) (1 ((11))) (2 ((12))))
         collecting (cons a b))
   '((0 . 10) (1 . 11) (2 . 12)))))
(define-test sacla-must-loop.734 (:tag :sacla)
 (assert-true
  (equal
   (loop for (a ((b)) c (((d)))) fixnum in
             '((0 ((10)) 20 (((30)))) (1 ((11)) 21 (((31))))
               (2 ((12)) 22 (((32)))))
         collecting (list a b c d))
   '((0 10 20 30) (1 11 21 31) (2 12 22 32)))))
(define-test sacla-must-loop.735 (:tag :sacla)
 (assert-true
  (equal
   (loop for (a ((b)) c (((d)))) of-type
             (fixnum ((fixnum)) fixnum (((fixnum)))) in
             '((0 ((10)) 20 (((30)))) (1 ((11)) 21 (((31))))
               (2 ((12)) 22 (((32)))))
         collecting (list a b c d))
   '((0 10 20 30) (1 11 21 31) (2 12 22 32)))))
(define-test sacla-must-loop.736 (:tag :sacla)
 (assert-true
  (equal
   (loop for (a nil nil (((b)))) of-type (fixnum nil nil (((fixnum)))) in
             '((0 ((10)) 20 (((30)))) (1 ((11)) 21 (((31))))
               (2 ((12)) 22 (((32)))))
         collecting (list a b))
   '((0 30) (1 31) (2 32)))))
(define-test sacla-must-loop.737 (:tag :sacla)
 (assert-true (equal (loop for (a) fixnum on '(0 1 2) collecting a) '(0 1 2))))
(define-test sacla-must-loop.738 (:tag :sacla)
 (assert-true
  (equal (loop for (a) of-type fixnum on '(0 1 2) collecting a) '(0 1 2))))
(define-test sacla-must-loop.739 (:tag :sacla)
 (assert-true
  (equal (loop for (a) float on '(0.3 1.3 2.3) collecting a) '(0.3 1.3 2.3))))
(define-test sacla-must-loop.740 (:tag :sacla)
 (assert-true
  (equal (loop for (a) of-type float on '(0.3 1.3 2.3) collecting a)
         '(0.3 1.3 2.3))))
(define-test sacla-must-loop.741 (:tag :sacla)
 (assert-true (equal (loop for (a) t on '(0 1 2) collecting a) '(0 1 2))))
(define-test sacla-must-loop.742 (:tag :sacla)
 (assert-true
  (equal (loop for (a) of-type t on '(0 1 2) collecting a) '(0 1 2))))
(define-test sacla-must-loop.743 (:tag :sacla)
 (assert-true
  (equal (loop for (a) of-type real on '(0 1.0 2/3) collecting a)
         '(0 1.0 2/3))))
(define-test sacla-must-loop.744 (:tag :sacla)
 (assert-true
  (equal (loop for (a nil b) fixnum on '(0 1 2) collecting (list a b))
         '((0 2) (1 nil) (2 nil)))))
(define-test sacla-must-loop.745 (:tag :sacla)
 (assert-true
  (equal
   (loop for (a nil b) of-type (fixnum nil fixnum) on '(0 1 2)
         collecting (list a b))
   '((0 2) (1 nil) (2 nil)))))
(define-test sacla-must-loop.746 (:tag :sacla)
 (assert-true
  (equal (loop for (nil . tail) t on '(0 1 2 3) append tail) '(1 2 3 2 3 3))))
(define-test sacla-must-loop.747 (:tag :sacla)
 (assert-true
  (equal (loop for (nil . tail) of-type t on '(0 1 2 3) append tail)
         '(1 2 3 2 3 3))))
(define-test sacla-must-loop.748 (:tag :sacla)
 (assert-true
  (equal (loop for (nil . tail) of-type list on '(0 1 2 3) append tail)
         '(1 2 3 2 3 3))))
(define-test sacla-must-loop.749 (:tag :sacla)
 (assert-true
  (equal (loop for (a b) t across #((x 0) (y 1) (z 2)) collecting (list b a))
         '((0 x) (1 y) (2 z)))))
(define-test sacla-must-loop.750 (:tag :sacla)
 (assert-true
  (equal
   (loop for (a b) of-type t across #((x 0) (y 1) (z 2)) collecting (list b a))
   '((0 x) (1 y) (2 z)))))
(define-test sacla-must-loop.751 (:tag :sacla)
 (assert-true
  (equal
   (loop for (a b) of-type ((member x y z) (member 0 1 2)) across
             #((x 0) (y 1) (z 2))
         collecting (list b a))
   '((0 x) (1 y) (2 z)))))
(define-test sacla-must-loop.752 (:tag :sacla)
 (assert-true
  (eq t (loop for (a) t := '(0) then (list (1+ a)) when (= a 3) return t))))
(define-test sacla-must-loop.753 (:tag :sacla)
 (assert-true
  (eq t
      (loop for (a) of-type t := '(0) then (list (1+ a))
            when (= a 3) return t))))
(define-test sacla-must-loop.754 (:tag :sacla)
 (assert-true
  (eq t
      (loop for (a) of-type (t) := '(0) then (list (1+ a))
            when (= a 3) return t))))
(define-test sacla-must-loop.755 (:tag :sacla)
 (assert-true
  (eq t
      (loop for (a) fixnum := '(0) then (list (1+ a)) when (= a 3) return t))))
(define-test sacla-must-loop.756 (:tag :sacla)
 (assert-true
  (eq t
      (loop for (a) of-type fixnum := '(0) then (list (1+ a))
            when (= a 3) return t))))
(define-test sacla-must-loop.757 (:tag :sacla)
 (assert-true
  (eq t
      (loop for (a) of-type (fixnum) := '(0) then (list (1+ a))
            when (= a 3) return t))))
(define-test sacla-must-loop.758 (:tag :sacla)
 (assert-true
  (eq t
      (loop for (a) float := '(0.0) then (list (1+ a))
            when (= a 3.0) return t))))
(define-test sacla-must-loop.759 (:tag :sacla)
 (assert-true
  (eq t
      (loop for (a) of-type float := '(0.0) then (list (1+ a))
            when (= a 3.0) return t))))
(define-test sacla-must-loop.760 (:tag :sacla)
 (assert-true
  (eq t
      (loop for (a) of-type (float) := '(0.0) then (list (1+ a))
            when (= a 3.0) return t))))
(define-test sacla-must-loop.761 (:tag :sacla)
 (assert-true
  (equal
   (loop for (a b) t := '(0 1) then (list (1+ b) (+ b 2))
         when (> a 5) do (loop-finish)
         collect (list a b))
   '((0 1) (2 3) (4 5)))))
(define-test sacla-must-loop.762 (:tag :sacla)
 (assert-true
  (equal
   (loop for (a b) of-type t := '(0 1) then (list (1+ b) (+ b 2))
         when (> a 5) do (loop-finish)
         collect (list a b))
   '((0 1) (2 3) (4 5)))))
(define-test sacla-must-loop.763 (:tag :sacla)
 (assert-true
  (equal
   (loop for (a b) of-type (t t) := '(0 1) then (list (1+ b) (+ b 2))
         when (> a 5) do (loop-finish)
         collect (list a b))
   '((0 1) (2 3) (4 5)))))
(define-test sacla-must-loop.764 (:tag :sacla)
 (assert-true
  (equal
   (loop for (a b) fixnum := '(0 1) then (list (1+ b) (+ b 2))
         when (> a 5) do (loop-finish)
         collect (list a b))
   '((0 1) (2 3) (4 5)))))
(define-test sacla-must-loop.765 (:tag :sacla)
 (assert-true
  (equal
   (loop for (a b) of-type fixnum := '(0 1) then (list (1+ b) (+ b 2))
         when (> a 5) do (loop-finish)
         collect (list a b))
   '((0 1) (2 3) (4 5)))))
(define-test sacla-must-loop.766 (:tag :sacla)
 (assert-true
  (equal
   (loop for (a b) of-type (fixnum fixnum) := '(0 1) then (list (1+ b) (+ b 2))
         when (> a 5) do (loop-finish)
         collect (list a b))
   '((0 1) (2 3) (4 5)))))
(define-test sacla-must-loop.767 (:tag :sacla)
 (assert-true
  (equal
   (loop for (a b) float := '(0.0 1.0) then (list (1+ b) (+ b 2.0))
         when (> a 5) do (loop-finish)
         collect (list a b))
   '((0.0 1.0) (2.0 3.0) (4.0 5.0)))))
(define-test sacla-must-loop.768 (:tag :sacla)
 (assert-true
  (equal
   (loop for (a b) of-type float := '(0.0 1.0) then (list (1+ b) (+ b 2.0))
         when (> a 5) do (loop-finish)
         collect (list a b))
   '((0.0 1.0) (2.0 3.0) (4.0 5.0)))))
(define-test sacla-must-loop.769 (:tag :sacla)
 (assert-true
  (equal
   (loop for (a b) of-type (float float) := '(0.0 1.0) then
             (list (1+ b) (+ b 2.0))
         when (> a 5) do (loop-finish)
         collect (list a b))
   '((0.0 1.0) (2.0 3.0) (4.0 5.0)))))
(define-test sacla-must-loop.770 (:tag :sacla)
 (assert-true
  (equal
   (loop for (a b) of-type (fixnum float) := '(0 1.0) then
             (list (+ a 2) (+ b 2.0))
         when (> a 5) do (loop-finish)
         collect (list a b))
   '((0 1.0) (2 3.0) (4 5.0)))))
(define-test sacla-must-loop.771 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table :test 'equal)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v))
          '((k0 0) (k1 1) (k2 2))
          '(v0 v1 v2))
    (loop for (k kn) t being each hash-key of table
          do (push (list k kn) stack))
    (null (set-exclusive-or stack '((k0 0) (k1 1) (k2 2)) :test #'equal)))))
(define-test sacla-must-loop.772 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table :test 'equal)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v))
          '((k0 0) (k1 1) (k2 2))
          '(v0 v1 v2))
    (loop for (k kn) of-type t being each hash-key of table
          do (push (list k kn) stack))
    (null (set-exclusive-or stack '((k0 0) (k1 1) (k2 2)) :test #'equal)))))
(define-test sacla-must-loop.773 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table :test 'equal)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v))
          '((k0 0) (k1 1) (k2 2))
          '(v0 v1 v2))
    (loop for (k kn) of-type (symbol fixnum) being each hash-key of table
          do (push (list k kn) stack))
    (null (set-exclusive-or stack '((k0 0) (k1 1) (k2 2)) :test #'equal)))))
(define-test sacla-must-loop.774 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table :test 'equal)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v))
          '((k0 0) (k1 1) (k2 2))
          '(v0 v1 v2))
    (loop for (k kn) of-type t being each hash-key of table
          do (push (list k kn) stack))
    (null (set-exclusive-or stack '((k0 0) (k1 1) (k2 2)) :test #'equal)))))
(define-test sacla-must-loop.775 (:tag :sacla)
 (assert-true
  (let ((table (make-hash-table :test 'equal)) stack)
    (mapc #'(lambda (k v) (setf (gethash k table) v))
          '((k0 0) (k1 1) (k2 2))
          '(v0 v1 v2))
    (loop for (k kn) of-type (t t) being each hash-key of table
          do (push (list k kn) stack))
    (null (set-exclusive-or stack '((k0 0) (k1 1) (k2 2)) :test #'equal)))))
(define-test sacla-must-loop.776 (:tag :sacla)
 (assert-true
  (handler-case
      (macroexpand '(loop with a = 0 for a downfrom 10 to 0 do (print a)))
    (program-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-loop.777 (:tag :sacla)
 (assert-true
  (handler-case (macroexpand '(loop for a from 0 upto 10 collect t into a))
    (program-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-loop.778 (:tag :sacla)
 (assert-true
  (= 4
     (loop for (item . x) of-type (t . fixnum) in '((a . 1) (b . 2) (c . 3))
           unless (eq item 'b) sum x))))
(define-test sacla-must-loop.779 (:tag :sacla)
 (assert-true
  (equal (loop for sublist on '(a b c d) collect sublist)
         '((a b c d) (b c d) (c d) (d)))))
(define-test sacla-must-loop.780 (:tag :sacla)
 (assert-true (equal (loop for (item) on '(1 2 3) collect item) '(1 2 3))))
(define-test sacla-must-loop.781 (:tag :sacla)
 (assert-true
  (equal
   (loop for item = 1 then (+ item 10) for iteration from 1 to 5 collect item)
   '(1 11 21 31 41))))
(define-test sacla-must-loop.782 (:tag :sacla)
 (assert-true
  (equal
   (loop for i below 3 collecting (loop for j below 2 collecting (list i j)))
   '(((0 0) (0 1)) ((1 0) (1 1)) ((2 0) (2 1))))))
(define-test sacla-must-loop.783 (:tag :sacla)
 (assert-true (zerop (loop for i from -10 upto 0 maximizing i))))
(define-test sacla-must-loop.784 (:tag :sacla)
 (assert-true
  (equal
   (loop for i from -10 upto 0
         maximizing i into max
         minimizing i into min
         finally (return (list max min)))
   '(0 -10))))
(define-test sacla-must-loop.785 (:tag :sacla)
 (assert-true
  (equal
   (loop for c across "aBcDeFg" when (and (upper-case-p c) c) collecting it)
   '(#\B #\D #\F))))
(define-test sacla-must-loop.786 (:tag :sacla)
 (assert-true
  (equal
   (loop named my-loop
         for i below 3
         collect i into x
         finally (return-from my-loop x))
   '(0 1 2))))
(define-test sacla-must-loop.787 (:tag :sacla)
 (assert-true
  (equal
   (loop named nil for i below 3 collect i into x finally (return-from nil x))
   '(0 1 2))))
(define-test sacla-must-loop.788 (:tag :sacla)
 (assert-true
  (equal (loop for i below 3 collect i into x finally (return-from nil x))
         '(0 1 2))))
(define-test sacla-must-loop.789 (:tag :sacla)
 (assert-true
  (equal (loop for i below 3 collect i into x finally (return x)) '(0 1 2))))
(define-test sacla-must-loop.790 (:tag :sacla)
 (assert-true
  (equal
   (loop for a from 10 above 0
         for b in '(1 2 3 4 5 6 7 8 9 10)
         for c on '(j k l m n o p q r s)
         for d = 100 then (1- d)
         collect (list a b (first c) d))
   '((10 1 j 100) (9 2 k 99) (8 3 l 98) (7 4 m 97) (6 5 n 96) (5 6 o 95)
     (4 7 p 94) (3 8 q 93) (2 9 r 92) (1 10 s 91)))))
(define-test sacla-must-loop.791 (:tag :sacla)
 (assert-true
  (equal
   (loop with e = 0
         for a from 10 above 0
         for b in '(1 2 3 4 5 6 7 8 9 10)
         for c on '(j k l m n o p q r s)
         for d = 100 then (1- d)
         append (list a b (first c) d) into values
         initially (setq e 1000)
         repeat 1
         finally (return (cons e values)))
   '(1000 10 1 j 100))))
(define-test sacla-must-loop.792 (:tag :sacla)
 (assert-true
  (equal
   (loop with e = 0
         for a from 10 above 0
         for b in '(1 2 3 4 5 6 7 8 9 10)
         for c on '(j k l m n o p q r s)
         for d = 100 then (1- d)
         append (list a b (first c) d) into values
         initially (setq e 1000)
         repeat 2
         finally (return (cons e values)))
   '(1000 10 1 j 100 9 2 k 99))))
(define-test sacla-must-loop.793 (:tag :sacla)
 (assert-true
  (equal
   (loop for a from 0 upto 100 by 2
         repeat 1000
         when (zerop (mod a 10)) collect a)
   '(0 10 20 30 40 50 60 70 80 90 100))))
(define-test sacla-must-loop.794 (:tag :sacla)
 (assert-true
  (let ((it '0))
    (equal
     (loop for a in '(nil x y nil z)
           when a
             collect it
             and collect it)
     '(x 0 y 0 z 0)))))
(define-test sacla-must-loop.795 (:tag :sacla)
 (assert-true
  (let ((it '0))
    (equal (loop for a in '(x nil y nil z nil) if a collect it end collect it)
           '(x 0 0 y 0 0 z 0 0)))))
(define-test sacla-must-loop.796 (:tag :sacla)
 (assert-true
  (subsetp '(car cdr list)
           (let (bag)
             (loop for sym being the external-symbols of 'common-lisp
                   do (push sym bag))
             bag))))
(define-test sacla-must-loop.797 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use nil)) bag)
      (and (null (loop for sym being the symbols of pkg do (push sym bag)))
           (null bag))))))
(define-test sacla-must-loop.798 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use nil)) bag)
      (and
       (null
        (loop for sym being the external-symbols of pkg do (push sym bag)))
       (null bag))))))
(define-test sacla-must-loop.799 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use nil)) bag)
      (and
       (null (loop for sym being the present-symbols of pkg do (push sym bag)))
       (null bag))))))
(define-test sacla-must-loop.800 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name)
           (push (intern name "TB-BAR-TO-USE") bag0)
           (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (push (intern name pkg) bag0)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop for sym being the symbols of pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.801 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name)
           (push (intern name "TB-BAR-TO-USE") bag0)
           (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (push (intern name pkg) bag0)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop for sym being each symbols of pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.802 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name)
           (push (intern name "TB-BAR-TO-USE") bag0)
           (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (push (intern name pkg) bag0)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop for sym being the symbol of pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.803 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name)
           (push (intern name "TB-BAR-TO-USE") bag0)
           (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (push (intern name pkg) bag0)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop for sym being each symbol of pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.804 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name)
           (push (intern name "TB-BAR-TO-USE") bag0)
           (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (push (intern name pkg) bag0)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop for sym being the symbols in pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.805 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name)
           (push (intern name "TB-BAR-TO-USE") bag0)
           (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (push (intern name pkg) bag0)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop for sym being each symbols in pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.806 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name)
           (push (intern name "TB-BAR-TO-USE") bag0)
           (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (push (intern name pkg) bag0)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop for sym being the symbol in pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.807 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name)
           (push (intern name "TB-BAR-TO-USE") bag0)
           (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (push (intern name pkg) bag0)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop for sym being each symbol in pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.808 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (push (intern name pkg) bag0)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop for sym being the present-symbols of pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.809 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (push (intern name pkg) bag0)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop for sym being each present-symbols of pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.810 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (push (intern name pkg) bag0)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop for sym being the present-symbol of pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.811 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (push (intern name pkg) bag0)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop for sym being each present-symbol of pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.812 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (push (intern name pkg) bag0)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop for sym being the present-symbols in pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.813 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (push (intern name pkg) bag0)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop for sym being each present-symbols in pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.814 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (push (intern name pkg) bag0)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop for sym being the present-symbol in pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.815 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (push (intern name pkg) bag0)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop for sym being each present-symbol in pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.816 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (intern name pkg)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop for sym being the external-symbols of pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.817 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (intern name pkg)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop for sym being each external-symbols of pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.818 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (intern name pkg)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop for sym being the external-symbol of pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.819 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (intern name pkg)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop for sym being each external-symbol of pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.820 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (intern name pkg)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop for sym being the external-symbols in pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.821 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (intern name pkg)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop for sym being each external-symbols in pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.822 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (intern name pkg)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop for sym being the external-symbol in pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.823 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (intern name pkg)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop for sym being each external-symbol in pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.824 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name)
           (push (intern name "TB-BAR-TO-USE") bag0)
           (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (push (intern name pkg) bag0)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop as sym being the symbols of pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.825 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name)
           (push (intern name "TB-BAR-TO-USE") bag0)
           (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (push (intern name pkg) bag0)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop as sym being each symbols of pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.826 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name)
           (push (intern name "TB-BAR-TO-USE") bag0)
           (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (push (intern name pkg) bag0)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop as sym being the symbol of pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.827 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name)
           (push (intern name "TB-BAR-TO-USE") bag0)
           (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (push (intern name pkg) bag0)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop as sym being each symbol of pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.828 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name)
           (push (intern name "TB-BAR-TO-USE") bag0)
           (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (push (intern name pkg) bag0)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop as sym being the symbols in pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.829 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name)
           (push (intern name "TB-BAR-TO-USE") bag0)
           (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (push (intern name pkg) bag0)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop as sym being each symbols in pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.830 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name)
           (push (intern name "TB-BAR-TO-USE") bag0)
           (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (push (intern name pkg) bag0)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop as sym being the symbol in pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.831 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name)
           (push (intern name "TB-BAR-TO-USE") bag0)
           (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (push (intern name pkg) bag0)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop as sym being each symbol in pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.832 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (push (intern name pkg) bag0)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop as sym being the present-symbols of pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.833 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (push (intern name pkg) bag0)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop as sym being each present-symbols of pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.834 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (push (intern name pkg) bag0)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop as sym being the present-symbol of pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.835 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (push (intern name pkg) bag0)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop as sym being each present-symbol of pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.836 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (push (intern name pkg) bag0)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop as sym being the present-symbols in pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.837 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (push (intern name pkg) bag0)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop as sym being each present-symbols in pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.838 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (push (intern name pkg) bag0)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop as sym being the present-symbol in pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.839 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (push (intern name pkg) bag0)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop as sym being each present-symbol in pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.840 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (intern name pkg)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop as sym being the external-symbols of pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.841 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (intern name pkg)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop as sym being each external-symbols of pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.842 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (intern name pkg)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop as sym being the external-symbol of pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.843 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (intern name pkg)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop as sym being each external-symbol of pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.844 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (intern name pkg)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop as sym being the external-symbols in pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.845 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (intern name pkg)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop as sym being each external-symbols in pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.846 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (intern name pkg)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop as sym being the external-symbol in pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.847 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (intern name pkg)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop as sym being each external-symbol in pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.848 (:tag :sacla)
 (assert-true
  (eq t (loop for symbol being the symbols of 'cl finally (return t)))))
(define-test sacla-must-loop.849 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (intern name pkg)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop as sym of-type symbol being the external-symbols of pkg
            do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.850 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (intern name pkg)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop as sym t being the external-symbols of pkg do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.851 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE")
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((pkg (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))) bag0 bag)
      (mapc
       #'(lambda (name) (export (intern name "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
       '("J" "K" "L"))
      (mapc #'(lambda (name) (intern name pkg)) '("A" "B" "C"))
      (mapc
       #'(lambda (name)
           (push (intern name pkg) bag0)
           (export (intern name pkg) pkg))
       '("X" "Y" "Z"))
      (loop as sym of-type t being the external-symbols of pkg
            do (push sym bag))
      (null (set-exclusive-or bag0 bag))))))
(define-test sacla-must-loop.852 (:tag :sacla)
 (assert-true
  (eq t
      (loop for c in '(#\A #\S #\Z #\a)
            always (eq t
                       (loop for s in
                                 (loop for s being the external-symbols of 'cl
                                       when (char= c (char (symbol-name s) 0))
                                         collect s)
                             always (char= c (char (symbol-name s) 0))))))))

