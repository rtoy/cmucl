(in-package #:sacla-lisp-unit)
(define-test sacla-must-character.1 (:tag :sacla) (assert-true (char= #\d #\d)))
(define-test sacla-must-character.2 (:tag :sacla)
 (assert-true (not (char= #\A #\a))))
(define-test sacla-must-character.3 (:tag :sacla)
 (assert-true (not (char= #\d #\x))))
(define-test sacla-must-character.4 (:tag :sacla)
 (assert-true (not (char= #\d #\D))))
(define-test sacla-must-character.5 (:tag :sacla)
 (assert-true (not (char/= #\d #\d))))
(define-test sacla-must-character.6 (:tag :sacla)
 (assert-true (char/= #\d #\x)))
(define-test sacla-must-character.7 (:tag :sacla)
 (assert-true (char/= #\d #\D)))
(define-test sacla-must-character.8 (:tag :sacla)
 (assert-true (char= #\d #\d #\d #\d)))
(define-test sacla-must-character.9 (:tag :sacla)
 (assert-true (not (char/= #\d #\d #\d #\d))))
(define-test sacla-must-character.10 (:tag :sacla)
 (assert-true (not (char= #\d #\d #\x #\d))))
(define-test sacla-must-character.11 (:tag :sacla)
 (assert-true (not (char/= #\d #\d #\x #\d))))
(define-test sacla-must-character.12 (:tag :sacla)
 (assert-true (not (char= #\d #\y #\x #\c))))
(define-test sacla-must-character.13 (:tag :sacla)
 (assert-true (char/= #\d #\y #\x #\c)))
(define-test sacla-must-character.14 (:tag :sacla)
 (assert-true (not (char= #\d #\c #\d))))
(define-test sacla-must-character.15 (:tag :sacla)
 (assert-true (not (char/= #\d #\c #\d))))
(define-test sacla-must-character.16 (:tag :sacla)
 (assert-true (char< #\d #\x)))
(define-test sacla-must-character.17 (:tag :sacla)
 (assert-true (char<= #\d #\x)))
(define-test sacla-must-character.18 (:tag :sacla)
 (assert-true (not (char< #\d #\d))))
(define-test sacla-must-character.19 (:tag :sacla)
 (assert-true (char<= #\d #\d)))
(define-test sacla-must-character.20 (:tag :sacla)
 (assert-true (char< #\a #\e #\y #\z)))
(define-test sacla-must-character.21 (:tag :sacla)
 (assert-true (char<= #\a #\e #\y #\z)))
(define-test sacla-must-character.22 (:tag :sacla)
 (assert-true (not (char< #\a #\e #\e #\y))))
(define-test sacla-must-character.23 (:tag :sacla)
 (assert-true (char<= #\a #\e #\e #\y)))
(define-test sacla-must-character.24 (:tag :sacla)
 (assert-true (char> #\e #\d)))
(define-test sacla-must-character.25 (:tag :sacla)
 (assert-true (char>= #\e #\d)))
(define-test sacla-must-character.26 (:tag :sacla)
 (assert-true (char> #\d #\c #\b #\a)))
(define-test sacla-must-character.27 (:tag :sacla)
 (assert-true (char>= #\d #\c #\b #\a)))
(define-test sacla-must-character.28 (:tag :sacla)
 (assert-true (not (char> #\d #\d #\c #\a))))
(define-test sacla-must-character.29 (:tag :sacla)
 (assert-true (char>= #\d #\d #\c #\a)))
(define-test sacla-must-character.30 (:tag :sacla)
 (assert-true (not (char> #\e #\d #\b #\c #\a))))
(define-test sacla-must-character.31 (:tag :sacla)
 (assert-true (not (char>= #\e #\d #\b #\c #\a))))
(define-test sacla-must-character.32 (:tag :sacla)
 (assert-true (char-equal #\A #\a)))
(define-test sacla-must-character.33 (:tag :sacla)
 (assert-true
  (equal (stable-sort (list #\b #\A #\B #\a #\c #\C) #'char-lessp)
         '(#\A #\a #\b #\B #\c #\C))))
(define-test sacla-must-character.34 (:tag :sacla) (assert-true (char= #\a)))
(define-test sacla-must-character.35 (:tag :sacla)
 (assert-true (char= #\a #\a)))
(define-test sacla-must-character.36 (:tag :sacla)
 (assert-true (char= #\a #\a #\a)))
(define-test sacla-must-character.37 (:tag :sacla)
 (assert-true (char= #\a #\a #\a #\a)))
(define-test sacla-must-character.38 (:tag :sacla)
 (assert-true (char= #\a #\a #\a #\a #\a)))
(define-test sacla-must-character.39 (:tag :sacla)
 (assert-true (char= #\a #\a #\a #\a #\a #\a)))
(define-test sacla-must-character.40 (:tag :sacla)
 (assert-true
  (let ((c #\z))
    (and (eq c c) (char= c c)))))
(define-test sacla-must-character.41 (:tag :sacla)
 (assert-true (not (char= #\Z #\z))))
(define-test sacla-must-character.42 (:tag :sacla)
 (assert-true (not (char= #\z #\z #\z #\a))))
(define-test sacla-must-character.43 (:tag :sacla)
 (assert-true (not (char= #\a #\z #\z #\z #\a))))
(define-test sacla-must-character.44 (:tag :sacla)
 (assert-true (not (char= #\z #\i #\z #\z))))
(define-test sacla-must-character.45 (:tag :sacla)
 (assert-true (not (char= #\z #\z #\Z #\z))))
(define-test sacla-must-character.46 (:tag :sacla) (assert-true (char/= #\a)))
(define-test sacla-must-character.47 (:tag :sacla)
 (assert-true (char/= #\a #\b)))
(define-test sacla-must-character.48 (:tag :sacla)
 (assert-true (char/= #\a #\b #\c)))
(define-test sacla-must-character.49 (:tag :sacla)
 (assert-true (char/= #\a #\b #\c #\d)))
(define-test sacla-must-character.50 (:tag :sacla)
 (assert-true (char/= #\a #\b #\c #\d #\e)))
(define-test sacla-must-character.51 (:tag :sacla)
 (assert-true (char/= #\a #\b #\c #\d #\e #\f)))
(define-test sacla-must-character.52 (:tag :sacla)
 (assert-true
  (let ((c #\z))
    (and (eq c c) (not (char/= c c))))))
(define-test sacla-must-character.53 (:tag :sacla)
 (assert-true (char/= #\Z #\z)))
(define-test sacla-must-character.54 (:tag :sacla)
 (assert-true (not (char/= #\z #\z #\z #\a))))
(define-test sacla-must-character.55 (:tag :sacla)
 (assert-true (not (char= #\a #\z #\z #\z #\a))))
(define-test sacla-must-character.56 (:tag :sacla)
 (assert-true (not (char= #\z #\i #\z #\z))))
(define-test sacla-must-character.57 (:tag :sacla)
 (assert-true (not (char= #\z #\z #\Z #\z))))
(define-test sacla-must-character.58 (:tag :sacla)
 (assert-true (not (char/= #\a #\a #\b #\c))))
(define-test sacla-must-character.59 (:tag :sacla)
 (assert-true (not (char/= #\a #\b #\a #\c))))
(define-test sacla-must-character.60 (:tag :sacla)
 (assert-true (not (char/= #\a #\b #\c #\a))))
(define-test sacla-must-character.61 (:tag :sacla) (assert-true (char< #\a)))
(define-test sacla-must-character.62 (:tag :sacla)
 (assert-true (char< #\a #\z)))
(define-test sacla-must-character.63 (:tag :sacla)
 (assert-true
  (char< #\a
         #\b
         #\c
         #\d
         #\e
         #\f
         #\g
         #\h
         #\i
         #\j
         #\k
         #\l
         #\m
         #\n
         #\o
         #\p
         #\q
         #\r
         #\s
         #\t
         #\u
         #\v
         #\w
         #\x
         #\y
         #\z)))
(define-test sacla-must-character.64 (:tag :sacla)
 (assert-true
  (not
   (char< #\z
          #\y
          #\x
          #\w
          #\v
          #\u
          #\t
          #\s
          #\r
          #\q
          #\p
          #\o
          #\n
          #\m
          #\l
          #\k
          #\j
          #\i
          #\h
          #\g
          #\f
          #\e
          #\d
          #\c
          #\b
          #\a))))
(define-test sacla-must-character.65 (:tag :sacla)
 (assert-true
  (char< #\A
         #\B
         #\C
         #\D
         #\E
         #\F
         #\G
         #\H
         #\I
         #\J
         #\K
         #\L
         #\M
         #\N
         #\O
         #\P
         #\Q
         #\R
         #\S
         #\T
         #\U
         #\V
         #\W
         #\X
         #\Y
         #\Z)))
(define-test sacla-must-character.66 (:tag :sacla)
 (assert-true
  (not
   (char< #\Z
          #\Y
          #\X
          #\W
          #\V
          #\U
          #\T
          #\S
          #\R
          #\Q
          #\P
          #\O
          #\N
          #\M
          #\L
          #\K
          #\J
          #\I
          #\H
          #\G
          #\F
          #\E
          #\D
          #\C
          #\B
          #\A))))
(define-test sacla-must-character.67 (:tag :sacla)
 (assert-true (char< #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
(define-test sacla-must-character.68 (:tag :sacla)
 (assert-true (not (char< #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2 #\1 #\0))))
(define-test sacla-must-character.69 (:tag :sacla)
 (assert-true (or (char< #\9 #\A) (char< #\Z #\0))))
(define-test sacla-must-character.70 (:tag :sacla)
 (assert-true (or (char< #\9 #\a) (char< #\z #\0))))
(define-test sacla-must-character.71 (:tag :sacla)
 (assert-true (not (char< #\a #\a #\b #\c))))
(define-test sacla-must-character.72 (:tag :sacla)
 (assert-true (not (char< #\a #\b #\a #\c))))
(define-test sacla-must-character.73 (:tag :sacla)
 (assert-true (not (char< #\a #\b #\c #\a))))
(define-test sacla-must-character.74 (:tag :sacla)
 (assert-true (not (char< #\9 #\0))))
(define-test sacla-must-character.75 (:tag :sacla) (assert-true (char> #\a)))
(define-test sacla-must-character.76 (:tag :sacla)
 (assert-true (not (char> #\a #\z))))
(define-test sacla-must-character.77 (:tag :sacla)
 (assert-true (char> #\z #\a)))
(define-test sacla-must-character.78 (:tag :sacla)
 (assert-true
  (not
   (char> #\a
          #\b
          #\c
          #\d
          #\e
          #\f
          #\g
          #\h
          #\i
          #\j
          #\k
          #\l
          #\m
          #\n
          #\o
          #\p
          #\q
          #\r
          #\s
          #\t
          #\u
          #\v
          #\w
          #\x
          #\y
          #\z))))
(define-test sacla-must-character.79 (:tag :sacla)
 (assert-true
  (char> #\z
         #\y
         #\x
         #\w
         #\v
         #\u
         #\t
         #\s
         #\r
         #\q
         #\p
         #\o
         #\n
         #\m
         #\l
         #\k
         #\j
         #\i
         #\h
         #\g
         #\f
         #\e
         #\d
         #\c
         #\b
         #\a)))
(define-test sacla-must-character.80 (:tag :sacla)
 (assert-true
  (not
   (char> #\A
          #\B
          #\C
          #\D
          #\E
          #\F
          #\G
          #\H
          #\I
          #\J
          #\K
          #\L
          #\M
          #\N
          #\O
          #\P
          #\Q
          #\R
          #\S
          #\T
          #\U
          #\V
          #\W
          #\X
          #\Y
          #\Z))))
(define-test sacla-must-character.81 (:tag :sacla)
 (assert-true
  (char> #\Z
         #\Y
         #\X
         #\W
         #\V
         #\U
         #\T
         #\S
         #\R
         #\Q
         #\P
         #\O
         #\N
         #\M
         #\L
         #\K
         #\J
         #\I
         #\H
         #\G
         #\F
         #\E
         #\D
         #\C
         #\B
         #\A)))
(define-test sacla-must-character.82 (:tag :sacla)
 (assert-true (not (char> #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))))
(define-test sacla-must-character.83 (:tag :sacla)
 (assert-true (char> #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2 #\1 #\0)))
(define-test sacla-must-character.84 (:tag :sacla)
 (assert-true (or (char> #\A #\9) (char> #\0 #\Z))))
(define-test sacla-must-character.85 (:tag :sacla)
 (assert-true (or (char> #\a #\9) (char> #\0 #\z))))
(define-test sacla-must-character.86 (:tag :sacla)
 (assert-true (not (char> #\a #\a #\b #\c))))
(define-test sacla-must-character.87 (:tag :sacla)
 (assert-true (not (char> #\a #\b #\a #\c))))
(define-test sacla-must-character.88 (:tag :sacla)
 (assert-true (not (char> #\a #\b #\c #\a))))
(define-test sacla-must-character.89 (:tag :sacla)
 (assert-true (char> #\9 #\0)))
(define-test sacla-must-character.90 (:tag :sacla) (assert-true (char<= #\a)))
(define-test sacla-must-character.91 (:tag :sacla)
 (assert-true (char<= #\a #\z)))
(define-test sacla-must-character.92 (:tag :sacla)
 (assert-true (char<= #\a #\a)))
(define-test sacla-must-character.93 (:tag :sacla)
 (assert-true (char<= #\Z #\Z)))
(define-test sacla-must-character.94 (:tag :sacla)
 (assert-true
  (char<= #\a
          #\b
          #\c
          #\d
          #\e
          #\f
          #\g
          #\h
          #\i
          #\j
          #\k
          #\l
          #\m
          #\n
          #\o
          #\p
          #\q
          #\r
          #\s
          #\t
          #\u
          #\v
          #\w
          #\x
          #\y
          #\z)))
(define-test sacla-must-character.95 (:tag :sacla)
 (assert-true
  (char<= #\a
          #\a
          #\b
          #\b
          #\c
          #\c
          #\d
          #\d
          #\e
          #\e
          #\f
          #\f
          #\g
          #\g
          #\h
          #\h
          #\i
          #\i
          #\j
          #\j
          #\k
          #\k
          #\l
          #\l
          #\m
          #\m
          #\n
          #\n
          #\o
          #\o
          #\p
          #\p
          #\q
          #\q
          #\r
          #\r
          #\s
          #\s
          #\t
          #\t
          #\u
          #\u
          #\v
          #\v
          #\w
          #\w
          #\x
          #\x
          #\y
          #\y
          #\z
          #\z)))
(define-test sacla-must-character.96 (:tag :sacla)
 (assert-true
  (not
   (char<= #\z
           #\y
           #\x
           #\w
           #\v
           #\u
           #\t
           #\s
           #\r
           #\q
           #\p
           #\o
           #\n
           #\m
           #\l
           #\k
           #\j
           #\i
           #\h
           #\g
           #\f
           #\e
           #\d
           #\c
           #\b
           #\a))))
(define-test sacla-must-character.97 (:tag :sacla)
 (assert-true
  (char<= #\A
          #\B
          #\C
          #\D
          #\E
          #\F
          #\G
          #\H
          #\I
          #\J
          #\K
          #\L
          #\M
          #\N
          #\O
          #\P
          #\Q
          #\R
          #\S
          #\T
          #\U
          #\V
          #\W
          #\X
          #\Y
          #\Z)))
(define-test sacla-must-character.98 (:tag :sacla)
 (assert-true
  (char<= #\A
          #\B
          #\B
          #\C
          #\D
          #\E
          #\E
          #\F
          #\G
          #\H
          #\I
          #\I
          #\J
          #\K
          #\L
          #\M
          #\N
          #\N
          #\O
          #\P
          #\Q
          #\R
          #\S
          #\T
          #\T
          #\U
          #\V
          #\W
          #\X
          #\Y
          #\Z)))
(define-test sacla-must-character.99 (:tag :sacla)
 (assert-true
  (not
   (char<= #\Z
           #\Y
           #\X
           #\W
           #\V
           #\U
           #\T
           #\S
           #\R
           #\Q
           #\P
           #\O
           #\N
           #\M
           #\L
           #\K
           #\J
           #\I
           #\H
           #\G
           #\F
           #\E
           #\D
           #\C
           #\B
           #\A))))
(define-test sacla-must-character.100 (:tag :sacla)
 (assert-true (char<= #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
(define-test sacla-must-character.101 (:tag :sacla)
 (assert-true
  (char<= #\0 #\1 #\2 #\2 #\3 #\3 #\3 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\9)))
(define-test sacla-must-character.102 (:tag :sacla)
 (assert-true (not (char<= #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2 #\1 #\0))))
(define-test sacla-must-character.103 (:tag :sacla)
 (assert-true (or (char<= #\9 #\A) (char<= #\Z #\0))))
(define-test sacla-must-character.104 (:tag :sacla)
 (assert-true (or (char<= #\9 #\a) (char<= #\z #\0))))
(define-test sacla-must-character.105 (:tag :sacla)
 (assert-true (char<= #\a #\a #\b #\c)))
(define-test sacla-must-character.106 (:tag :sacla)
 (assert-true (not (char<= #\a #\b #\a #\c))))
(define-test sacla-must-character.107 (:tag :sacla)
 (assert-true (not (char<= #\a #\b #\c #\a))))
(define-test sacla-must-character.108 (:tag :sacla)
 (assert-true (not (char<= #\9 #\0))))
(define-test sacla-must-character.109 (:tag :sacla) (assert-true (char>= #\a)))
(define-test sacla-must-character.110 (:tag :sacla)
 (assert-true (not (char>= #\a #\z))))
(define-test sacla-must-character.111 (:tag :sacla)
 (assert-true (char>= #\z #\a)))
(define-test sacla-must-character.112 (:tag :sacla)
 (assert-true (char>= #\a #\a)))
(define-test sacla-must-character.113 (:tag :sacla)
 (assert-true (char>= #\Z #\Z)))
(define-test sacla-must-character.114 (:tag :sacla)
 (assert-true
  (not
   (char>= #\a
           #\b
           #\c
           #\d
           #\e
           #\f
           #\g
           #\h
           #\i
           #\j
           #\k
           #\l
           #\m
           #\n
           #\o
           #\p
           #\q
           #\r
           #\s
           #\t
           #\u
           #\v
           #\w
           #\x
           #\y
           #\z))))
(define-test sacla-must-character.115 (:tag :sacla)
 (assert-true
  (char>= #\z
          #\y
          #\x
          #\w
          #\v
          #\u
          #\t
          #\s
          #\r
          #\q
          #\p
          #\o
          #\n
          #\m
          #\l
          #\k
          #\j
          #\i
          #\h
          #\g
          #\f
          #\e
          #\d
          #\c
          #\b
          #\a)))
(define-test sacla-must-character.116 (:tag :sacla)
 (assert-true
  (char>= #\z
          #\z
          #\y
          #\x
          #\w
          #\v
          #\u
          #\t
          #\s
          #\r
          #\q
          #\p
          #\o
          #\n
          #\n
          #\m
          #\m
          #\l
          #\k
          #\j
          #\i
          #\h
          #\g
          #\f
          #\e
          #\d
          #\c
          #\b
          #\a
          #\a)))
(define-test sacla-must-character.117 (:tag :sacla)
 (assert-true
  (not
   (char>= #\A
           #\B
           #\C
           #\D
           #\E
           #\F
           #\G
           #\H
           #\I
           #\J
           #\K
           #\L
           #\M
           #\N
           #\O
           #\P
           #\Q
           #\R
           #\S
           #\T
           #\U
           #\V
           #\W
           #\X
           #\Y
           #\Z))))
(define-test sacla-must-character.118 (:tag :sacla)
 (assert-true
  (char>= #\Z
          #\Y
          #\X
          #\W
          #\V
          #\U
          #\T
          #\S
          #\R
          #\Q
          #\P
          #\O
          #\N
          #\M
          #\L
          #\K
          #\J
          #\I
          #\H
          #\G
          #\F
          #\E
          #\D
          #\C
          #\B
          #\A)))
(define-test sacla-must-character.119 (:tag :sacla)
 (assert-true
  (char>= #\Z
          #\Y
          #\X
          #\W
          #\V
          #\U
          #\U
          #\T
          #\T
          #\S
          #\S
          #\R
          #\Q
          #\P
          #\O
          #\N
          #\M
          #\L
          #\K
          #\J
          #\I
          #\H
          #\H
          #\G
          #\G
          #\F
          #\F
          #\E
          #\D
          #\C
          #\B
          #\A)))
(define-test sacla-must-character.120 (:tag :sacla)
 (assert-true (not (char>= #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))))
(define-test sacla-must-character.121 (:tag :sacla)
 (assert-true (char>= #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2 #\1 #\0)))
(define-test sacla-must-character.122 (:tag :sacla)
 (assert-true (char>= #\9 #\8 #\8 #\8 #\7 #\6 #\5 #\4 #\3 #\3 #\3 #\2 #\1 #\0)))
(define-test sacla-must-character.123 (:tag :sacla)
 (assert-true (or (char>= #\A #\9) (char>= #\0 #\Z))))
(define-test sacla-must-character.124 (:tag :sacla)
 (assert-true (or (char>= #\a #\9) (char>= #\0 #\z))))
(define-test sacla-must-character.125 (:tag :sacla)
 (assert-true (char>= #\c #\b #\a #\a)))
(define-test sacla-must-character.126 (:tag :sacla)
 (assert-true (not (char>= #\c #\b #\a #\a #\b #\c))))
(define-test sacla-must-character.127 (:tag :sacla)
 (assert-true (not (char>= #\c #\b #\a #\c))))
(define-test sacla-must-character.128 (:tag :sacla)
 (assert-true (not (char>= #\c #\b #\c #\a))))
(define-test sacla-must-character.129 (:tag :sacla)
 (assert-true (char>= #\9 #\0)))
(define-test sacla-must-character.130 (:tag :sacla)
 (assert-true (not (char>= #\0 #\9))))
(define-test sacla-must-character.131 (:tag :sacla)
 (assert-true (char-equal #\a)))
(define-test sacla-must-character.132 (:tag :sacla)
 (assert-true (char-equal #\a #\a)))
(define-test sacla-must-character.133 (:tag :sacla)
 (assert-true (char-equal #\a #\a #\a)))
(define-test sacla-must-character.134 (:tag :sacla)
 (assert-true (char-equal #\a #\a #\a #\a)))
(define-test sacla-must-character.135 (:tag :sacla)
 (assert-true (char-equal #\a #\a #\a #\a #\a)))
(define-test sacla-must-character.136 (:tag :sacla)
 (assert-true (char-equal #\a #\a #\a #\a #\a #\a)))
(define-test sacla-must-character.137 (:tag :sacla)
 (assert-true (char-equal #\a #\A)))
(define-test sacla-must-character.138 (:tag :sacla)
 (assert-true (char-equal #\a #\A #\a)))
(define-test sacla-must-character.139 (:tag :sacla)
 (assert-true (char-equal #\a #\a #\A #\a)))
(define-test sacla-must-character.140 (:tag :sacla)
 (assert-true (char-equal #\a #\a #\a #\A #\a)))
(define-test sacla-must-character.141 (:tag :sacla)
 (assert-true (char-equal #\a #\a #\a #\a #\A #\a)))
(define-test sacla-must-character.142 (:tag :sacla)
 (assert-true
  (let ((c #\z))
    (and (eq c c) (char-equal c c)))))
(define-test sacla-must-character.143 (:tag :sacla)
 (assert-true (char-equal #\Z #\z)))
(define-test sacla-must-character.144 (:tag :sacla)
 (assert-true (not (char-equal #\z #\z #\z #\a))))
(define-test sacla-must-character.145 (:tag :sacla)
 (assert-true (not (char-equal #\a #\z #\z #\z #\a))))
(define-test sacla-must-character.146 (:tag :sacla)
 (assert-true (not (char-equal #\z #\i #\z #\z))))
(define-test sacla-must-character.147 (:tag :sacla)
 (assert-true (char-equal #\z #\z #\Z #\z)))
(define-test sacla-must-character.148 (:tag :sacla)
 (assert-true (char-equal #\a #\A #\a #\A #\a #\A #\a #\A #\a #\A)))
(define-test sacla-must-character.149 (:tag :sacla)
 (assert-true (char-not-equal #\a)))
(define-test sacla-must-character.150 (:tag :sacla)
 (assert-true (char-not-equal #\a #\b)))
(define-test sacla-must-character.151 (:tag :sacla)
 (assert-true (char-not-equal #\a #\b #\c)))
(define-test sacla-must-character.152 (:tag :sacla)
 (assert-true (char-not-equal #\a #\b #\c #\d)))
(define-test sacla-must-character.153 (:tag :sacla)
 (assert-true (char-not-equal #\a #\b #\c #\d #\e)))
(define-test sacla-must-character.154 (:tag :sacla)
 (assert-true (char-not-equal #\a #\b #\c #\d #\e #\f)))
(define-test sacla-must-character.155 (:tag :sacla)
 (assert-true
  (let ((c #\z))
    (and (eq c c) (not (char-not-equal c c))))))
(define-test sacla-must-character.156 (:tag :sacla)
 (assert-true (not (char-not-equal #\Z #\z))))
(define-test sacla-must-character.157 (:tag :sacla)
 (assert-true (not (char-not-equal #\z #\z #\z #\a))))
(define-test sacla-must-character.158 (:tag :sacla)
 (assert-true (not (char= #\a #\z #\z #\z #\a))))
(define-test sacla-must-character.159 (:tag :sacla)
 (assert-true (not (char= #\z #\i #\z #\z))))
(define-test sacla-must-character.160 (:tag :sacla)
 (assert-true (not (char= #\z #\z #\Z #\z))))
(define-test sacla-must-character.161 (:tag :sacla)
 (assert-true (not (char-not-equal #\a #\a #\b #\c))))
(define-test sacla-must-character.162 (:tag :sacla)
 (assert-true (not (char-not-equal #\a #\b #\a #\c))))
(define-test sacla-must-character.163 (:tag :sacla)
 (assert-true (not (char-not-equal #\a #\b #\c #\a))))
(define-test sacla-must-character.164 (:tag :sacla)
 (assert-true (not (char-not-equal #\a #\A #\a #\A))))
(define-test sacla-must-character.165 (:tag :sacla)
 (assert-true (char-lessp #\a)))
(define-test sacla-must-character.166 (:tag :sacla)
 (assert-true (char-lessp #\a #\z)))
(define-test sacla-must-character.167 (:tag :sacla)
 (assert-true
  (char-lessp #\a
              #\b
              #\c
              #\d
              #\e
              #\f
              #\g
              #\h
              #\i
              #\j
              #\k
              #\l
              #\m
              #\n
              #\o
              #\p
              #\q
              #\r
              #\s
              #\t
              #\u
              #\v
              #\w
              #\x
              #\y
              #\z)))
(define-test sacla-must-character.168 (:tag :sacla)
 (assert-true
  (not
   (char-lessp #\z
               #\y
               #\x
               #\w
               #\v
               #\u
               #\t
               #\s
               #\r
               #\q
               #\p
               #\o
               #\n
               #\m
               #\l
               #\k
               #\j
               #\i
               #\h
               #\g
               #\f
               #\e
               #\d
               #\c
               #\b
               #\a))))
(define-test sacla-must-character.169 (:tag :sacla)
 (assert-true
  (char-lessp #\A
              #\B
              #\C
              #\D
              #\E
              #\F
              #\G
              #\H
              #\I
              #\J
              #\K
              #\L
              #\M
              #\N
              #\O
              #\P
              #\Q
              #\R
              #\S
              #\T
              #\U
              #\V
              #\W
              #\X
              #\Y
              #\Z)))
(define-test sacla-must-character.170 (:tag :sacla)
 (assert-true
  (not
   (char-lessp #\Z
               #\Y
               #\X
               #\W
               #\V
               #\U
               #\T
               #\S
               #\R
               #\Q
               #\P
               #\O
               #\N
               #\M
               #\L
               #\K
               #\J
               #\I
               #\H
               #\G
               #\F
               #\E
               #\D
               #\C
               #\B
               #\A))))
(define-test sacla-must-character.171 (:tag :sacla)
 (assert-true
  (char-lessp #\a
              #\B
              #\c
              #\D
              #\e
              #\F
              #\g
              #\H
              #\i
              #\J
              #\k
              #\L
              #\m
              #\N
              #\o
              #\P
              #\q
              #\R
              #\s
              #\T
              #\u
              #\V
              #\w
              #\X
              #\y
              #\Z)))
(define-test sacla-must-character.172 (:tag :sacla)
 (assert-true (char-lessp #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
(define-test sacla-must-character.173 (:tag :sacla)
 (assert-true (not (char-lessp #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2 #\1 #\0))))
(define-test sacla-must-character.174 (:tag :sacla)
 (assert-true (or (char-lessp #\9 #\A) (char-lessp #\Z #\0))))
(define-test sacla-must-character.175 (:tag :sacla)
 (assert-true (or (char-lessp #\9 #\a) (char-lessp #\z #\0))))
(define-test sacla-must-character.176 (:tag :sacla)
 (assert-true (not (char-lessp #\a #\a #\b #\c))))
(define-test sacla-must-character.177 (:tag :sacla)
 (assert-true (not (char-lessp #\a #\b #\a #\c))))
(define-test sacla-must-character.178 (:tag :sacla)
 (assert-true (not (char-lessp #\a #\b #\c #\a))))
(define-test sacla-must-character.179 (:tag :sacla)
 (assert-true (not (char-lessp #\9 #\0))))
(define-test sacla-must-character.180 (:tag :sacla)
 (assert-true (and (char-lessp #\a #\Z) (char-lessp #\A #\z))))
(define-test sacla-must-character.181 (:tag :sacla)
 (assert-true (char-greaterp #\a)))
(define-test sacla-must-character.182 (:tag :sacla)
 (assert-true (not (char-greaterp #\a #\z))))
(define-test sacla-must-character.183 (:tag :sacla)
 (assert-true (char-greaterp #\z #\a)))
(define-test sacla-must-character.184 (:tag :sacla)
 (assert-true
  (not
   (char-greaterp #\a
                  #\b
                  #\c
                  #\d
                  #\e
                  #\f
                  #\g
                  #\h
                  #\i
                  #\j
                  #\k
                  #\l
                  #\m
                  #\n
                  #\o
                  #\p
                  #\q
                  #\r
                  #\s
                  #\t
                  #\u
                  #\v
                  #\w
                  #\x
                  #\y
                  #\z))))
(define-test sacla-must-character.185 (:tag :sacla)
 (assert-true
  (char-greaterp #\z
                 #\y
                 #\x
                 #\w
                 #\v
                 #\u
                 #\t
                 #\s
                 #\r
                 #\q
                 #\p
                 #\o
                 #\n
                 #\m
                 #\l
                 #\k
                 #\j
                 #\i
                 #\h
                 #\g
                 #\f
                 #\e
                 #\d
                 #\c
                 #\b
                 #\a)))
(define-test sacla-must-character.186 (:tag :sacla)
 (assert-true
  (not
   (char-greaterp #\A
                  #\B
                  #\C
                  #\D
                  #\E
                  #\F
                  #\G
                  #\H
                  #\I
                  #\J
                  #\K
                  #\L
                  #\M
                  #\N
                  #\O
                  #\P
                  #\Q
                  #\R
                  #\S
                  #\T
                  #\U
                  #\V
                  #\W
                  #\X
                  #\Y
                  #\Z))))
(define-test sacla-must-character.187 (:tag :sacla)
 (assert-true
  (char-greaterp #\Z
                 #\Y
                 #\X
                 #\W
                 #\V
                 #\U
                 #\T
                 #\S
                 #\R
                 #\Q
                 #\P
                 #\O
                 #\N
                 #\M
                 #\L
                 #\K
                 #\J
                 #\I
                 #\H
                 #\G
                 #\F
                 #\E
                 #\D
                 #\C
                 #\B
                 #\A)))
(define-test sacla-must-character.188 (:tag :sacla)
 (assert-true
  (char-greaterp #\z
                 #\Y
                 #\x
                 #\W
                 #\v
                 #\U
                 #\t
                 #\S
                 #\r
                 #\Q
                 #\p
                 #\O
                 #\n
                 #\M
                 #\l
                 #\K
                 #\j
                 #\I
                 #\h
                 #\G
                 #\f
                 #\E
                 #\d
                 #\C
                 #\b
                 #\A)))
(define-test sacla-must-character.189 (:tag :sacla)
 (assert-true (not (char-greaterp #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))))
(define-test sacla-must-character.190 (:tag :sacla)
 (assert-true (char-greaterp #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2 #\1 #\0)))
(define-test sacla-must-character.191 (:tag :sacla)
 (assert-true (or (char-greaterp #\A #\9) (char-greaterp #\0 #\Z))))
(define-test sacla-must-character.192 (:tag :sacla)
 (assert-true (or (char-greaterp #\a #\9) (char-greaterp #\0 #\z))))
(define-test sacla-must-character.193 (:tag :sacla)
 (assert-true (not (char-greaterp #\a #\a #\b #\c))))
(define-test sacla-must-character.194 (:tag :sacla)
 (assert-true (not (char-greaterp #\a #\b #\a #\c))))
(define-test sacla-must-character.195 (:tag :sacla)
 (assert-true (not (char-greaterp #\a #\b #\c #\a))))
(define-test sacla-must-character.196 (:tag :sacla)
 (assert-true (char-greaterp #\9 #\0)))
(define-test sacla-must-character.197 (:tag :sacla)
 (assert-true (and (char-greaterp #\z #\A) (char-greaterp #\Z #\a))))
(define-test sacla-must-character.198 (:tag :sacla)
 (assert-true (char-not-greaterp #\a)))
(define-test sacla-must-character.199 (:tag :sacla)
 (assert-true (char-not-greaterp #\a #\z)))
(define-test sacla-must-character.200 (:tag :sacla)
 (assert-true (char-not-greaterp #\a #\a)))
(define-test sacla-must-character.201 (:tag :sacla)
 (assert-true (char-not-greaterp #\Z #\Z)))
(define-test sacla-must-character.202 (:tag :sacla)
 (assert-true
  (char-not-greaterp #\a
                     #\b
                     #\c
                     #\d
                     #\e
                     #\f
                     #\g
                     #\h
                     #\i
                     #\j
                     #\k
                     #\l
                     #\m
                     #\n
                     #\o
                     #\p
                     #\q
                     #\r
                     #\s
                     #\t
                     #\u
                     #\v
                     #\w
                     #\x
                     #\y
                     #\z)))
(define-test sacla-must-character.203 (:tag :sacla)
 (assert-true
  (char-not-greaterp #\a
                     #\a
                     #\b
                     #\b
                     #\c
                     #\c
                     #\d
                     #\d
                     #\e
                     #\e
                     #\f
                     #\f
                     #\g
                     #\g
                     #\h
                     #\h
                     #\i
                     #\i
                     #\j
                     #\j
                     #\k
                     #\k
                     #\l
                     #\l
                     #\m
                     #\m
                     #\n
                     #\n
                     #\o
                     #\o
                     #\p
                     #\p
                     #\q
                     #\q
                     #\r
                     #\r
                     #\s
                     #\s
                     #\t
                     #\t
                     #\u
                     #\u
                     #\v
                     #\v
                     #\w
                     #\w
                     #\x
                     #\x
                     #\y
                     #\y
                     #\z
                     #\z)))
(define-test sacla-must-character.204 (:tag :sacla)
 (assert-true
  (char-not-greaterp #\a
                     #\A
                     #\b
                     #\B
                     #\c
                     #\C
                     #\d
                     #\D
                     #\e
                     #\E
                     #\f
                     #\F
                     #\g
                     #\G
                     #\h
                     #\H
                     #\i
                     #\I
                     #\j
                     #\J
                     #\k
                     #\K
                     #\l
                     #\L
                     #\m
                     #\M
                     #\n
                     #\N
                     #\o
                     #\O
                     #\p
                     #\P
                     #\q
                     #\Q
                     #\r
                     #\R
                     #\s
                     #\S
                     #\t
                     #\T
                     #\u
                     #\U
                     #\v
                     #\V
                     #\w
                     #\W
                     #\x
                     #\X
                     #\y
                     #\Y
                     #\z
                     #\z)))
(define-test sacla-must-character.205 (:tag :sacla)
 (assert-true
  (not
   (char-not-greaterp #\z
                      #\y
                      #\x
                      #\w
                      #\v
                      #\u
                      #\t
                      #\s
                      #\r
                      #\q
                      #\p
                      #\o
                      #\n
                      #\m
                      #\l
                      #\k
                      #\j
                      #\i
                      #\h
                      #\g
                      #\f
                      #\e
                      #\d
                      #\c
                      #\b
                      #\a))))
(define-test sacla-must-character.206 (:tag :sacla)
 (assert-true
  (char-not-greaterp #\A
                     #\B
                     #\C
                     #\D
                     #\E
                     #\F
                     #\G
                     #\H
                     #\I
                     #\J
                     #\K
                     #\L
                     #\M
                     #\N
                     #\O
                     #\P
                     #\Q
                     #\R
                     #\S
                     #\T
                     #\U
                     #\V
                     #\W
                     #\X
                     #\Y
                     #\Z)))
(define-test sacla-must-character.207 (:tag :sacla)
 (assert-true
  (char-not-greaterp #\A
                     #\B
                     #\B
                     #\C
                     #\D
                     #\E
                     #\E
                     #\F
                     #\G
                     #\H
                     #\I
                     #\I
                     #\J
                     #\K
                     #\L
                     #\M
                     #\N
                     #\N
                     #\O
                     #\P
                     #\Q
                     #\R
                     #\S
                     #\T
                     #\T
                     #\U
                     #\V
                     #\W
                     #\X
                     #\Y
                     #\Z)))
(define-test sacla-must-character.208 (:tag :sacla)
 (assert-true
  (not
   (char-not-greaterp #\Z
                      #\Y
                      #\X
                      #\W
                      #\V
                      #\U
                      #\T
                      #\S
                      #\R
                      #\Q
                      #\P
                      #\O
                      #\N
                      #\M
                      #\L
                      #\K
                      #\J
                      #\I
                      #\H
                      #\G
                      #\F
                      #\E
                      #\D
                      #\C
                      #\B
                      #\A))))
(define-test sacla-must-character.209 (:tag :sacla)
 (assert-true (char-not-greaterp #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
(define-test sacla-must-character.210 (:tag :sacla)
 (assert-true
  (char-not-greaterp #\0
                     #\1
                     #\2
                     #\2
                     #\3
                     #\3
                     #\3
                     #\3
                     #\4
                     #\5
                     #\6
                     #\7
                     #\8
                     #\9
                     #\9)))
(define-test sacla-must-character.211 (:tag :sacla)
 (assert-true
  (not (char-not-greaterp #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2 #\1 #\0))))
(define-test sacla-must-character.212 (:tag :sacla)
 (assert-true (or (char-not-greaterp #\9 #\A) (char-not-greaterp #\Z #\0))))
(define-test sacla-must-character.213 (:tag :sacla)
 (assert-true (or (char-not-greaterp #\9 #\a) (char-not-greaterp #\z #\0))))
(define-test sacla-must-character.214 (:tag :sacla)
 (assert-true (char-not-greaterp #\a #\a #\b #\c)))
(define-test sacla-must-character.215 (:tag :sacla)
 (assert-true (not (char-not-greaterp #\a #\b #\a #\c))))
(define-test sacla-must-character.216 (:tag :sacla)
 (assert-true (not (char-not-greaterp #\a #\b #\c #\a))))
(define-test sacla-must-character.217 (:tag :sacla)
 (assert-true (not (char-not-greaterp #\9 #\0))))
(define-test sacla-must-character.218 (:tag :sacla)
 (assert-true (and (char-not-greaterp #\A #\z) (char-not-greaterp #\a #\Z))))
(define-test sacla-must-character.219 (:tag :sacla)
 (assert-true (char-not-lessp #\a)))
(define-test sacla-must-character.220 (:tag :sacla)
 (assert-true (not (char-not-lessp #\a #\z))))
(define-test sacla-must-character.221 (:tag :sacla)
 (assert-true (char-not-lessp #\z #\a)))
(define-test sacla-must-character.222 (:tag :sacla)
 (assert-true (char-not-lessp #\a #\a)))
(define-test sacla-must-character.223 (:tag :sacla)
 (assert-true (char-not-lessp #\Z #\Z)))
(define-test sacla-must-character.224 (:tag :sacla)
 (assert-true
  (not
   (char-not-lessp #\a
                   #\b
                   #\c
                   #\d
                   #\e
                   #\f
                   #\g
                   #\h
                   #\i
                   #\j
                   #\k
                   #\l
                   #\m
                   #\n
                   #\o
                   #\p
                   #\q
                   #\r
                   #\s
                   #\t
                   #\u
                   #\v
                   #\w
                   #\x
                   #\y
                   #\z))))
(define-test sacla-must-character.225 (:tag :sacla)
 (assert-true
  (char-not-lessp #\z
                  #\y
                  #\x
                  #\w
                  #\v
                  #\u
                  #\t
                  #\s
                  #\r
                  #\q
                  #\p
                  #\o
                  #\n
                  #\m
                  #\l
                  #\k
                  #\j
                  #\i
                  #\h
                  #\g
                  #\f
                  #\e
                  #\d
                  #\c
                  #\b
                  #\a)))
(define-test sacla-must-character.226 (:tag :sacla)
 (assert-true
  (char-not-lessp #\z
                  #\z
                  #\y
                  #\x
                  #\w
                  #\v
                  #\u
                  #\t
                  #\s
                  #\r
                  #\q
                  #\p
                  #\o
                  #\n
                  #\n
                  #\m
                  #\m
                  #\l
                  #\k
                  #\j
                  #\i
                  #\h
                  #\g
                  #\f
                  #\e
                  #\d
                  #\c
                  #\b
                  #\a
                  #\a)))
(define-test sacla-must-character.227 (:tag :sacla)
 (assert-true
  (not
   (char-not-lessp #\A
                   #\B
                   #\C
                   #\D
                   #\E
                   #\F
                   #\G
                   #\H
                   #\I
                   #\J
                   #\K
                   #\L
                   #\M
                   #\N
                   #\O
                   #\P
                   #\Q
                   #\R
                   #\S
                   #\T
                   #\U
                   #\V
                   #\W
                   #\X
                   #\Y
                   #\Z))))
(define-test sacla-must-character.228 (:tag :sacla)
 (assert-true
  (char-not-lessp #\Z
                  #\Y
                  #\X
                  #\W
                  #\V
                  #\U
                  #\T
                  #\S
                  #\R
                  #\Q
                  #\P
                  #\O
                  #\N
                  #\M
                  #\L
                  #\K
                  #\J
                  #\I
                  #\H
                  #\G
                  #\F
                  #\E
                  #\D
                  #\C
                  #\B
                  #\A)))
(define-test sacla-must-character.229 (:tag :sacla)
 (assert-true
  (char-not-lessp #\Z
                  #\Y
                  #\X
                  #\W
                  #\V
                  #\U
                  #\U
                  #\T
                  #\T
                  #\S
                  #\S
                  #\R
                  #\Q
                  #\P
                  #\O
                  #\N
                  #\M
                  #\L
                  #\K
                  #\J
                  #\I
                  #\H
                  #\H
                  #\G
                  #\G
                  #\F
                  #\F
                  #\E
                  #\D
                  #\C
                  #\B
                  #\A)))
(define-test sacla-must-character.230 (:tag :sacla)
 (assert-true
  (char-not-lessp #\z
                  #\Z
                  #\y
                  #\x
                  #\w
                  #\V
                  #\v
                  #\u
                  #\t
                  #\s
                  #\r
                  #\q
                  #\p
                  #\o
                  #\n
                  #\n
                  #\m
                  #\M
                  #\l
                  #\k
                  #\K
                  #\j
                  #\i
                  #\h
                  #\g
                  #\f
                  #\e
                  #\d
                  #\c
                  #\b
                  #\A
                  #\a)))
(define-test sacla-must-character.231 (:tag :sacla)
 (assert-true (not (char-not-lessp #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))))
(define-test sacla-must-character.232 (:tag :sacla)
 (assert-true (char-not-lessp #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2 #\1 #\0)))
(define-test sacla-must-character.233 (:tag :sacla)
 (assert-true
  (char-not-lessp #\9 #\8 #\8 #\8 #\7 #\6 #\5 #\4 #\3 #\3 #\3 #\2 #\1 #\0)))
(define-test sacla-must-character.234 (:tag :sacla)
 (assert-true (or (char-not-lessp #\A #\9) (char-not-lessp #\0 #\Z))))
(define-test sacla-must-character.235 (:tag :sacla)
 (assert-true (or (char-not-lessp #\a #\9) (char-not-lessp #\0 #\z))))
(define-test sacla-must-character.236 (:tag :sacla)
 (assert-true (char-not-lessp #\c #\b #\a #\a)))
(define-test sacla-must-character.237 (:tag :sacla)
 (assert-true (not (char-not-lessp #\c #\b #\a #\a #\b #\c))))
(define-test sacla-must-character.238 (:tag :sacla)
 (assert-true (not (char-not-lessp #\c #\b #\a #\c))))
(define-test sacla-must-character.239 (:tag :sacla)
 (assert-true (not (char-not-lessp #\c #\b #\c #\a))))
(define-test sacla-must-character.240 (:tag :sacla)
 (assert-true (char-not-lessp #\9 #\0)))
(define-test sacla-must-character.241 (:tag :sacla)
 (assert-true (not (char-not-lessp #\0 #\9))))
(define-test sacla-must-character.242 (:tag :sacla)
 (assert-true (and (char-not-lessp #\z #\A) (char-not-lessp #\Z #\a))))
(define-test sacla-must-character.243 (:tag :sacla)
 (assert-true (char= (character #\a) #\a)))
(define-test sacla-must-character.244 (:tag :sacla)
 (assert-true (char= (character #\b) #\b)))
(define-test sacla-must-character.245 (:tag :sacla)
 (assert-true (char= (character #\ ) #\ )))
(define-test sacla-must-character.246 (:tag :sacla)
 (assert-true (char= (character "a") #\a)))
(define-test sacla-must-character.247 (:tag :sacla)
 (assert-true (char= (character "X") #\X)))
(define-test sacla-must-character.248 (:tag :sacla)
 (assert-true (char= (character "z") #\z)))
(define-test sacla-must-character.249 (:tag :sacla)
 (assert-true (char= (character 'a) #\A)))
(define-test sacla-must-character.250 (:tag :sacla)
 (assert-true (char= (character '|a|) #\a)))
(define-test sacla-must-character.251 (:tag :sacla)
 (assert-true (alpha-char-p #\a)))
(define-test sacla-must-character.252 (:tag :sacla)
 (assert-true
  (every #'alpha-char-p
         '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q
           #\r #\s #\t #\u #\v #\w #\x #\y #\z))))
(define-test sacla-must-character.253 (:tag :sacla)
 (assert-true
  (every #'alpha-char-p
         '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q
           #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))))
(define-test sacla-must-character.254 (:tag :sacla)
 (assert-true
  (notany #'alpha-char-p '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))))
(define-test sacla-must-character.255 (:tag :sacla)
 (assert-true (not (alpha-char-p #\Newline))))
(define-test sacla-must-character.256 (:tag :sacla)
 (assert-true (alphanumericp #\Z)))
(define-test sacla-must-character.257 (:tag :sacla)
 (assert-true (alphanumericp #\9)))
(define-test sacla-must-character.258 (:tag :sacla)
 (assert-true
  (every #'alphanumericp
         '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q
           #\r #\s #\t #\u #\v #\w #\x #\y #\z))))
(define-test sacla-must-character.259 (:tag :sacla)
 (assert-true
  (every #'alphanumericp
         '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q
           #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))))
(define-test sacla-must-character.260 (:tag :sacla)
 (assert-true
  (every #'alphanumericp '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))))
(define-test sacla-must-character.261 (:tag :sacla)
 (assert-true (not (alphanumericp #\Newline))))
(define-test sacla-must-character.262 (:tag :sacla)
 (assert-true (not (alphanumericp #\#))))
(define-test sacla-must-character.263 (:tag :sacla)
 (assert-true (char= (digit-char 0) #\0)))
(define-test sacla-must-character.264 (:tag :sacla)
 (assert-true (char= (digit-char 10 11) #\A)))
(define-test sacla-must-character.265 (:tag :sacla)
 (assert-true (null (digit-char 10 10))))
(define-test sacla-must-character.266 (:tag :sacla)
 (assert-true (char= (digit-char 7) #\7)))
(define-test sacla-must-character.267 (:tag :sacla)
 (assert-true (null (digit-char 12))))
(define-test sacla-must-character.268 (:tag :sacla)
 (assert-true (char= (digit-char 12 16) #\C)))
(define-test sacla-must-character.269 (:tag :sacla)
 (assert-true (null (digit-char 6 2))))
(define-test sacla-must-character.270 (:tag :sacla)
 (assert-true (char= (digit-char 1 2) #\1)))
(define-test sacla-must-character.271 (:tag :sacla)
 (assert-true (char= (digit-char 35 36) #\Z)))
(define-test sacla-must-character.272 (:tag :sacla)
 (assert-true
  (do ((radix 2 (1+ radix)))
      ((= radix 37) t)
    (unless
        (dotimes (i radix t)
          (unless
              (char= (digit-char i radix)
                     (svref
                      #(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D
                        #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R
                        #\S #\T #\U #\V #\W #\X #\Y #\Z)
                      i))
            (return nil)))
      (return nil)))))
(define-test sacla-must-character.273 (:tag :sacla)
 (assert-true (= (digit-char-p #\0) 0)))
(define-test sacla-must-character.274 (:tag :sacla)
 (assert-true (= (digit-char-p #\5) 5)))
(define-test sacla-must-character.275 (:tag :sacla)
 (assert-true (not (digit-char-p #\5 2))))
(define-test sacla-must-character.276 (:tag :sacla)
 (assert-true (not (digit-char-p #\A))))
(define-test sacla-must-character.277 (:tag :sacla)
 (assert-true (not (digit-char-p #\a))))
(define-test sacla-must-character.278 (:tag :sacla)
 (assert-true (= (digit-char-p #\A 11) 10)))
(define-test sacla-must-character.279 (:tag :sacla)
 (assert-true (= (digit-char-p #\a 11) 10)))
(define-test sacla-must-character.280 (:tag :sacla)
 (assert-true
  (equal
   (mapcar
    #'(lambda (radix)
        (map 'list #'(lambda (x) (digit-char-p x radix)) "059AaFGZ"))
    '(2 8 10 16 36))
   '((0 nil nil nil nil nil nil nil) (0 5 nil nil nil nil nil nil)
     (0 5 9 nil nil nil nil nil) (0 5 9 10 10 15 nil nil)
     (0 5 9 10 10 15 16 35)))))
(define-test sacla-must-character.281 (:tag :sacla)
 (assert-true
  (do ((radix 2 (1+ radix)))
      ((= radix 37) t)
    (unless
        (dotimes (i radix t)
          (unless
              (=
               (digit-char-p (schar "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" i)
                             radix)
               i)
            (return nil)))
      (return nil)))))
(define-test sacla-must-character.282 (:tag :sacla)
 (assert-true
  (do ((radix 2 (1+ radix)))
      ((= radix 37) t)
    (unless
        (dotimes (i radix t)
          (unless
              (=
               (digit-char-p (schar "0123456789abcdefghijklmnopqrstuvwxyz" i)
                             radix)
               i)
            (return nil)))
      (return nil)))))
(define-test sacla-must-character.283 (:tag :sacla)
 (assert-true (graphic-char-p #\G)))
(define-test sacla-must-character.284 (:tag :sacla)
 (assert-true (graphic-char-p #\#)))
(define-test sacla-must-character.285 (:tag :sacla)
 (assert-true (graphic-char-p #\ )))
(define-test sacla-must-character.286 (:tag :sacla)
 (assert-true (not (graphic-char-p #\Newline))))
(define-test sacla-must-character.287 (:tag :sacla)
 (assert-true (standard-char-p #\a)))
(define-test sacla-must-character.288 (:tag :sacla)
 (assert-true (standard-char-p #\z)))
(define-test sacla-must-character.289 (:tag :sacla)
 (assert-true (standard-char-p #\Newline)))
(define-test sacla-must-character.290 (:tag :sacla)
 (assert-true
  (every #'standard-char-p
         " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_'abcdefghijklmnopqrstuvwxyz{|}~
")))
(define-test sacla-must-character.291 (:tag :sacla)
 (assert-true (char= (char-upcase #\a) #\A)))
(define-test sacla-must-character.292 (:tag :sacla)
 (assert-true (char= (char-upcase #\A) #\A)))
(define-test sacla-must-character.293 (:tag :sacla)
 (assert-true (char= (char-upcase #\-) #\-)))
(define-test sacla-must-character.294 (:tag :sacla)
 (assert-true (char= (char-downcase #\A) #\a)))
(define-test sacla-must-character.295 (:tag :sacla)
 (assert-true (char= (char-downcase #\a) #\a)))
(define-test sacla-must-character.296 (:tag :sacla)
 (assert-true (char= (char-downcase #\-) #\-)))
(define-test sacla-must-character.297 (:tag :sacla)
 (assert-true (not (upper-case-p #\a))))
(define-test sacla-must-character.298 (:tag :sacla)
 (assert-true (upper-case-p #\A)))
(define-test sacla-must-character.299 (:tag :sacla)
 (assert-true (not (upper-case-p #\-))))
(define-test sacla-must-character.300 (:tag :sacla)
 (assert-true (not (lower-case-p #\A))))
(define-test sacla-must-character.301 (:tag :sacla)
 (assert-true (lower-case-p #\a)))
(define-test sacla-must-character.302 (:tag :sacla)
 (assert-true (not (lower-case-p #\-))))
(define-test sacla-must-character.303 (:tag :sacla)
 (assert-true (both-case-p #\a)))
(define-test sacla-must-character.304 (:tag :sacla)
 (assert-true (both-case-p #\A)))
(define-test sacla-must-character.305 (:tag :sacla)
 (assert-true (not (both-case-p #\-))))
(define-test sacla-must-character.306 (:tag :sacla)
 (assert-true
  (let ((chars
         " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_'abcdefghijklmnopqrstuvwxyz{|}~
")
        c)
    (dotimes (i (length chars) t)
      (setq c (schar chars i))
      (cond
        ((upper-case-p c)
         (unless
             (and (both-case-p c)
                  (not (lower-case-p c))
                  (char= (char-upcase c) c)
                  (not (char= (char-downcase c) c)))
           (return nil)))
        ((lower-case-p c)
         (unless
             (and (both-case-p c)
                  (char= (char-downcase c) c)
                  (not (char= (char-upcase c) c)))
           (return nil)))
        (t
         (unless
             (and (not (upper-case-p c))
                  (not (lower-case-p c))
                  (not (both-case-p c))
                  (char= (char-upcase c) c)
                  (char= (char-downcase c) c))
           (return nil))))))))
(define-test sacla-must-character.307 (:tag :sacla)
 (assert-true
  (every (complement #'minusp)
         (map 'list
              #'char-code
              " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_'abcdefghijklmnopqrstuvwxyz{|}~
"))))
(define-test sacla-must-character.308 (:tag :sacla)
 (assert-true
  (every (complement #'minusp)
         (map 'list
              #'char-int
              " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_'abcdefghijklmnopqrstuvwxyz{|}~
"))))
(define-test sacla-must-character.309 (:tag :sacla)
 (assert-true
  (every #'characterp
         (map 'list
              #'code-char
              (map 'list
                   #'char-code
                   " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_'abcdefghijklmnopqrstuvwxyz{|}~
")))))
(define-test sacla-must-character.310 (:tag :sacla)
 (assert-true
  (dotimes (i char-code-limit t)
    (unless (or (null (code-char i)) (characterp (code-char i)))
      (return nil)))))
(define-test sacla-must-character.311 (:tag :sacla)
 (assert-true (char= #\  (name-char (char-name #\ )))))
(define-test sacla-must-character.312 (:tag :sacla)
 (assert-true (char= #\  (name-char (char-name #\ )))))
(define-test sacla-must-character.313 (:tag :sacla)
 (assert-true (char= #\Newline (name-char (char-name #\Newline)))))

