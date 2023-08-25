(in-package #:sacla-lisp-unit)
(define-test sacla-must-reader.1 (:tag :sacla)
 (assert-true
  (symbolp
   (read-from-string
    "|ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!$\"'(),_-./:;?+<=>#%&*@[\\]{|}`^~|"))))
(define-test sacla-must-reader.2 (:tag :sacla)
 (assert-true (eq (read-from-string "this") 'this)))
(define-test sacla-must-reader.3 (:tag :sacla)
 (assert-true (eq (read-from-string "cl:car") 'car)))
(define-test sacla-must-reader.4 (:tag :sacla)
 (assert-true (eq (read-from-string ":ok") :ok)))
(define-test sacla-must-reader.5 (:tag :sacla)
 (assert-true (eq (read-from-string "ok#") '|OK#|)))
(define-test sacla-must-reader.6 (:tag :sacla)
 (assert-true (eq (read-from-string "x#x") '|X#X|)))
(define-test sacla-must-reader.7 (:tag :sacla)
 (assert-true (eq (read-from-string "abc(x y z)") 'abc)))
(define-test sacla-must-reader.8 (:tag :sacla)
 (assert-true
  (multiple-value-bind (obj pos)
      (read-from-string "abc(x y z)")
    (and (eq obj 'abc)
         (equal (read-from-string "abc(x y z)" t nil :start pos) '(x y z))))))
(define-test sacla-must-reader.9 (:tag :sacla)
 (assert-true (eq (read-from-string "abc") (read-from-string "ABC"))))
(define-test sacla-must-reader.10 (:tag :sacla)
 (assert-true (eq (read-from-string "abc") (read-from-string "|ABC|"))))
(define-test sacla-must-reader.11 (:tag :sacla)
 (assert-true (eq (read-from-string "abc") (read-from-string "a|B|c"))))
(define-test sacla-must-reader.12 (:tag :sacla)
 (assert-true (not (eq (read-from-string "abc") (read-from-string "|abc|")))))
(define-test sacla-must-reader.13 (:tag :sacla)
 (assert-true (eq (read-from-string "abc") (read-from-string "\\A\\B\\C"))))
(define-test sacla-must-reader.14 (:tag :sacla)
 (assert-true (eq (read-from-string "abc") (read-from-string "a\\Bc"))))
(define-test sacla-must-reader.15 (:tag :sacla)
 (assert-true (eq (read-from-string "abc") (read-from-string "\\ABC"))))
(define-test sacla-must-reader.16 (:tag :sacla)
 (assert-true (not (eq (read-from-string "abc") (read-from-string "\\abc")))))
(define-test sacla-must-reader.17 (:tag :sacla)
 (assert-true (= 1 (eval (read-from-string "(length '(this-that))")))))
(define-test sacla-must-reader.18 (:tag :sacla)
 (assert-true (= 3 (eval (read-from-string "(length '(this - that))")))))
(define-test sacla-must-reader.19 (:tag :sacla)
 (assert-true
  (= 2
     (eval
      (read-from-string "(length '(a
   	b))")))))
(define-test sacla-must-reader.20 (:tag :sacla)
 (assert-true (= 34 (eval (read-from-string "(+ 34)")))))
(define-test sacla-must-reader.21 (:tag :sacla)
 (assert-true (= 7 (eval (read-from-string "(+ 3 4)")))))
(define-test sacla-must-reader.22 (:tag :sacla)
 (assert-true
  (eq
   :key (let ((*package* (find-package "KEYWORD")))
          (read-from-string "key")))))
(define-test sacla-must-reader.23 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package 'test-foo)
      (delete-package 'test-foo))
    (let ((*package* (make-package 'test-foo :use nil)))
      (and (not (find-symbol "BAR"))
           (eq (read-from-string "bar") (find-symbol "BAR")))))))
(define-test sacla-must-reader.24 (:tag :sacla)
 (assert-true (= (read-from-string "1.0") 1.0)))
(define-test sacla-must-reader.25 (:tag :sacla)
 (assert-true (= (read-from-string "2/3") 2/3)))
(define-test sacla-must-reader.26 (:tag :sacla)
 (assert-true (zerop (read-from-string "0"))))
(define-test sacla-must-reader.27 (:tag :sacla)
 (assert-true (zerop (read-from-string "0.0"))))
(define-test sacla-must-reader.28 (:tag :sacla)
 (assert-true (zerop (read-from-string "0/3"))))
(define-test sacla-must-reader.29 (:tag :sacla)
 (assert-true (null (read-from-string "()"))))
(define-test sacla-must-reader.30 (:tag :sacla)
 (assert-true (equal (read-from-string "(a)") '(a))))
(define-test sacla-must-reader.31 (:tag :sacla)
 (assert-true (equal (read-from-string "(a b)") '(a b))))
(define-test sacla-must-reader.32 (:tag :sacla)
 (assert-true (equal (read-from-string "(a b c)") '(a b c))))
(define-test sacla-must-reader.33 (:tag :sacla)
 (assert-true (equal (read-from-string "(a b c d)") '(a b c d))))
(define-test sacla-must-reader.34 (:tag :sacla)
 (assert-true (equal (read-from-string "(a b c d e)") '(a b c d e))))
(define-test sacla-must-reader.35 (:tag :sacla)
 (assert-true (equal (read-from-string "(a b c d e f)") '(a b c d e f))))
(define-test sacla-must-reader.36 (:tag :sacla)
 (assert-true (equal (read-from-string "(a b c d e f g)") '(a b c d e f g))))
(define-test sacla-must-reader.37 (:tag :sacla)
 (assert-true
  (equal (read-from-string "(a b c d e f g h)") '(a b c d e f g h))))
(define-test sacla-must-reader.38 (:tag :sacla)
 (assert-true
  (handler-case (read-from-string ".")
    (reader-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-reader.39 (:tag :sacla)
 (assert-true
  (handler-case (read-from-string "...")
    (reader-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-reader.40 (:tag :sacla)
 (assert-true
  (let ((*read-base* 8))
    (= (read-from-string "0") 0))))
(define-test sacla-must-reader.41 (:tag :sacla)
 (assert-true
  (let ((*read-base* 8))
    (= (read-from-string "1") 1))))
(define-test sacla-must-reader.42 (:tag :sacla)
 (assert-true
  (let ((*read-base* 8))
    (= (read-from-string "2") 2))))
(define-test sacla-must-reader.43 (:tag :sacla)
 (assert-true
  (let ((*read-base* 8))
    (= (read-from-string "3") 3))))
(define-test sacla-must-reader.44 (:tag :sacla)
 (assert-true
  (let ((*read-base* 8))
    (= (read-from-string "4") 4))))
(define-test sacla-must-reader.45 (:tag :sacla)
 (assert-true
  (let ((*read-base* 8))
    (= (read-from-string "5") 5))))
(define-test sacla-must-reader.46 (:tag :sacla)
 (assert-true
  (let ((*read-base* 8))
    (= (read-from-string "6") 6))))
(define-test sacla-must-reader.47 (:tag :sacla)
 (assert-true
  (let ((*read-base* 8))
    (= (read-from-string "7") 7))))
(define-test sacla-must-reader.48 (:tag :sacla)
 (assert-true
  (let ((*read-base* 8))
    (= (read-from-string "8.") 8))))
(define-test sacla-must-reader.49 (:tag :sacla)
 (assert-true
  (let ((*read-base* 8))
    (= (read-from-string "10") 8))))
(define-test sacla-must-reader.50 (:tag :sacla)
 (assert-true
  (let ((*read-base* 8))
    (= (read-from-string "11") 9))))
(define-test sacla-must-reader.51 (:tag :sacla)
 (assert-true
  (let ((*read-base* 8))
    (= (read-from-string "12") 10))))
(define-test sacla-must-reader.52 (:tag :sacla)
 (assert-true
  (let ((*read-base* 8))
    (= (read-from-string "13") 11))))
(define-test sacla-must-reader.53 (:tag :sacla)
 (assert-true
  (let ((*read-base* 8))
    (= (read-from-string "14") 12))))
(define-test sacla-must-reader.54 (:tag :sacla)
 (assert-true
  (let ((*read-base* 8))
    (= (read-from-string "15") 13))))
(define-test sacla-must-reader.55 (:tag :sacla)
 (assert-true
  (let ((*read-base* 8))
    (= (read-from-string "16") 14))))
(define-test sacla-must-reader.56 (:tag :sacla)
 (assert-true
  (let ((*read-base* 8))
    (= (read-from-string "17") 15))))
(define-test sacla-must-reader.57 (:tag :sacla)
 (assert-true
  (let ((*read-base* 8))
    (= (read-from-string "20") 16))))
(define-test sacla-must-reader.58 (:tag :sacla)
 (assert-true
  (let ((*read-base* 8))
    (= (read-from-string "21") 17))))
(define-test sacla-must-reader.59 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "0") 0))))
(define-test sacla-must-reader.60 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "1") 1))))
(define-test sacla-must-reader.61 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "2") 2))))
(define-test sacla-must-reader.62 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "3") 3))))
(define-test sacla-must-reader.63 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "4") 4))))
(define-test sacla-must-reader.64 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "5") 5))))
(define-test sacla-must-reader.65 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "6") 6))))
(define-test sacla-must-reader.66 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "7") 7))))
(define-test sacla-must-reader.67 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "8") 8))))
(define-test sacla-must-reader.68 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "9") 9))))
(define-test sacla-must-reader.69 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "A") 10))))
(define-test sacla-must-reader.70 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "a") 10))))
(define-test sacla-must-reader.71 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "B") 11))))
(define-test sacla-must-reader.72 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "b") 11))))
(define-test sacla-must-reader.73 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "C") 12))))
(define-test sacla-must-reader.74 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "c") 12))))
(define-test sacla-must-reader.75 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "D") 13))))
(define-test sacla-must-reader.76 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "d") 13))))
(define-test sacla-must-reader.77 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "E") 14))))
(define-test sacla-must-reader.78 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "e") 14))))
(define-test sacla-must-reader.79 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "F") 15))))
(define-test sacla-must-reader.80 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "f") 15))))
(define-test sacla-must-reader.81 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "10") 16))))
(define-test sacla-must-reader.82 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "11") 17))))
(define-test sacla-must-reader.83 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "12") 18))))
(define-test sacla-must-reader.84 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "13") 19))))
(define-test sacla-must-reader.85 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "14") 20))))
(define-test sacla-must-reader.86 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "15") 21))))
(define-test sacla-must-reader.87 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "16") 22))))
(define-test sacla-must-reader.88 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "17") 23))))
(define-test sacla-must-reader.89 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "18") 24))))
(define-test sacla-must-reader.90 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "19") 25))))
(define-test sacla-must-reader.91 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "1A") 26))))
(define-test sacla-must-reader.92 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "1a") 26))))
(define-test sacla-must-reader.93 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "1B") 27))))
(define-test sacla-must-reader.94 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "1b") 27))))
(define-test sacla-must-reader.95 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "1C") 28))))
(define-test sacla-must-reader.96 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "1c") 28))))
(define-test sacla-must-reader.97 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "1D") 29))))
(define-test sacla-must-reader.98 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "1d") 29))))
(define-test sacla-must-reader.99 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "1E") 30))))
(define-test sacla-must-reader.100 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "1e") 30))))
(define-test sacla-must-reader.101 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "1F") 31))))
(define-test sacla-must-reader.102 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "1f") 31))))
(define-test sacla-must-reader.103 (:tag :sacla)
 (assert-true
  (let ((*read-base* 16))
    (= (read-from-string "20") 32))))
(define-test sacla-must-reader.104 (:tag :sacla)
 (assert-true (= (read-from-string "0") 0)))
(define-test sacla-must-reader.105 (:tag :sacla)
 (assert-true (= (read-from-string "+0") 0)))
(define-test sacla-must-reader.106 (:tag :sacla)
 (assert-true (= (read-from-string "-0") 0)))
(define-test sacla-must-reader.107 (:tag :sacla)
 (assert-true (integerp (read-from-string "0"))))
(define-test sacla-must-reader.108 (:tag :sacla)
 (assert-true (integerp (read-from-string "+0"))))
(define-test sacla-must-reader.109 (:tag :sacla)
 (assert-true (integerp (read-from-string "-0"))))
(define-test sacla-must-reader.110 (:tag :sacla)
 (assert-true (= (read-from-string "1") 1)))
(define-test sacla-must-reader.111 (:tag :sacla)
 (assert-true (= (read-from-string "+1") 1)))
(define-test sacla-must-reader.112 (:tag :sacla)
 (assert-true (= (read-from-string "-1") -1)))
(define-test sacla-must-reader.113 (:tag :sacla)
 (assert-true (integerp (read-from-string "1"))))
(define-test sacla-must-reader.114 (:tag :sacla)
 (assert-true (integerp (read-from-string "+1"))))
(define-test sacla-must-reader.115 (:tag :sacla)
 (assert-true (integerp (read-from-string "-1"))))
(define-test sacla-must-reader.116 (:tag :sacla)
 (assert-true (= (read-from-string "12") 12)))
(define-test sacla-must-reader.117 (:tag :sacla)
 (assert-true (= (read-from-string "+12") 12)))
(define-test sacla-must-reader.118 (:tag :sacla)
 (assert-true (= (read-from-string "-12") -12)))
(define-test sacla-must-reader.119 (:tag :sacla)
 (assert-true (integerp (read-from-string "12"))))
(define-test sacla-must-reader.120 (:tag :sacla)
 (assert-true (integerp (read-from-string "+12"))))
(define-test sacla-must-reader.121 (:tag :sacla)
 (assert-true (integerp (read-from-string "-12"))))
(define-test sacla-must-reader.122 (:tag :sacla)
 (assert-true (= (read-from-string "123") 123)))
(define-test sacla-must-reader.123 (:tag :sacla)
 (assert-true (= (read-from-string "+123") 123)))
(define-test sacla-must-reader.124 (:tag :sacla)
 (assert-true (= (read-from-string "-123") -123)))
(define-test sacla-must-reader.125 (:tag :sacla)
 (assert-true (integerp (read-from-string "123"))))
(define-test sacla-must-reader.126 (:tag :sacla)
 (assert-true (integerp (read-from-string "+123"))))
(define-test sacla-must-reader.127 (:tag :sacla)
 (assert-true (integerp (read-from-string "-123"))))
(define-test sacla-must-reader.128 (:tag :sacla)
 (assert-true (= (read-from-string "1234") 1234)))
(define-test sacla-must-reader.129 (:tag :sacla)
 (assert-true (= (read-from-string "+1234") 1234)))
(define-test sacla-must-reader.130 (:tag :sacla)
 (assert-true (= (read-from-string "-1234") -1234)))
(define-test sacla-must-reader.131 (:tag :sacla)
 (assert-true (integerp (read-from-string "1234"))))
(define-test sacla-must-reader.132 (:tag :sacla)
 (assert-true (integerp (read-from-string "+1234"))))
(define-test sacla-must-reader.133 (:tag :sacla)
 (assert-true (integerp (read-from-string "-1234"))))
(define-test sacla-must-reader.134 (:tag :sacla)
 (assert-true (= (read-from-string "12345") 12345)))
(define-test sacla-must-reader.135 (:tag :sacla)
 (assert-true (= (read-from-string "+12345") 12345)))
(define-test sacla-must-reader.136 (:tag :sacla)
 (assert-true (= (read-from-string "-12345") -12345)))
(define-test sacla-must-reader.137 (:tag :sacla)
 (assert-true (integerp (read-from-string "12345"))))
(define-test sacla-must-reader.138 (:tag :sacla)
 (assert-true (integerp (read-from-string "+12345"))))
(define-test sacla-must-reader.139 (:tag :sacla)
 (assert-true (integerp (read-from-string "-12345"))))
(define-test sacla-must-reader.140 (:tag :sacla)
 (assert-true (integerp (read-from-string "48148148031244413808971345"))))
(define-test sacla-must-reader.141 (:tag :sacla)
 (assert-true (integerp (read-from-string "+48148148031244413808971345"))))
(define-test sacla-must-reader.142 (:tag :sacla)
 (assert-true (integerp (read-from-string "-48148148031244413808971345"))))
(define-test sacla-must-reader.143 (:tag :sacla)
 (assert-true (= (read-from-string "0.") 0)))
(define-test sacla-must-reader.144 (:tag :sacla)
 (assert-true (= (read-from-string "+0.") 0)))
(define-test sacla-must-reader.145 (:tag :sacla)
 (assert-true (= (read-from-string "-0.") 0)))
(define-test sacla-must-reader.146 (:tag :sacla)
 (assert-true (integerp (read-from-string "0."))))
(define-test sacla-must-reader.147 (:tag :sacla)
 (assert-true (integerp (read-from-string "+0."))))
(define-test sacla-must-reader.148 (:tag :sacla)
 (assert-true (integerp (read-from-string "-0."))))
(define-test sacla-must-reader.149 (:tag :sacla)
 (assert-true (= (read-from-string "1.") 1)))
(define-test sacla-must-reader.150 (:tag :sacla)
 (assert-true (= (read-from-string "+1.") 1)))
(define-test sacla-must-reader.151 (:tag :sacla)
 (assert-true (= (read-from-string "-1.") -1)))
(define-test sacla-must-reader.152 (:tag :sacla)
 (assert-true (integerp (read-from-string "1."))))
(define-test sacla-must-reader.153 (:tag :sacla)
 (assert-true (integerp (read-from-string "+1."))))
(define-test sacla-must-reader.154 (:tag :sacla)
 (assert-true (integerp (read-from-string "-1."))))
(define-test sacla-must-reader.155 (:tag :sacla)
 (assert-true (= (read-from-string "12.") 12)))
(define-test sacla-must-reader.156 (:tag :sacla)
 (assert-true (= (read-from-string "+12.") 12)))
(define-test sacla-must-reader.157 (:tag :sacla)
 (assert-true (= (read-from-string "-12.") -12)))
(define-test sacla-must-reader.158 (:tag :sacla)
 (assert-true (integerp (read-from-string "12."))))
(define-test sacla-must-reader.159 (:tag :sacla)
 (assert-true (integerp (read-from-string "+12."))))
(define-test sacla-must-reader.160 (:tag :sacla)
 (assert-true (integerp (read-from-string "-12."))))
(define-test sacla-must-reader.161 (:tag :sacla)
 (assert-true (= (read-from-string "123.") 123)))
(define-test sacla-must-reader.162 (:tag :sacla)
 (assert-true (= (read-from-string "+123.") 123)))
(define-test sacla-must-reader.163 (:tag :sacla)
 (assert-true (= (read-from-string "-123.") -123)))
(define-test sacla-must-reader.164 (:tag :sacla)
 (assert-true (integerp (read-from-string "123."))))
(define-test sacla-must-reader.165 (:tag :sacla)
 (assert-true (integerp (read-from-string "+123."))))
(define-test sacla-must-reader.166 (:tag :sacla)
 (assert-true (integerp (read-from-string "-123."))))
(define-test sacla-must-reader.167 (:tag :sacla)
 (assert-true (= (read-from-string "1234.") 1234)))
(define-test sacla-must-reader.168 (:tag :sacla)
 (assert-true (= (read-from-string "+1234.") 1234)))
(define-test sacla-must-reader.169 (:tag :sacla)
 (assert-true (= (read-from-string "-1234.") -1234)))
(define-test sacla-must-reader.170 (:tag :sacla)
 (assert-true (integerp (read-from-string "1234."))))
(define-test sacla-must-reader.171 (:tag :sacla)
 (assert-true (integerp (read-from-string "+1234."))))
(define-test sacla-must-reader.172 (:tag :sacla)
 (assert-true (integerp (read-from-string "-1234."))))
(define-test sacla-must-reader.173 (:tag :sacla)
 (assert-true (= (read-from-string "12345.") 12345)))
(define-test sacla-must-reader.174 (:tag :sacla)
 (assert-true (= (read-from-string "+12345.") 12345)))
(define-test sacla-must-reader.175 (:tag :sacla)
 (assert-true (= (read-from-string "-12345.") -12345)))
(define-test sacla-must-reader.176 (:tag :sacla)
 (assert-true (integerp (read-from-string "12345."))))
(define-test sacla-must-reader.177 (:tag :sacla)
 (assert-true (integerp (read-from-string "+12345."))))
(define-test sacla-must-reader.178 (:tag :sacla)
 (assert-true (integerp (read-from-string "-12345."))))
(define-test sacla-must-reader.179 (:tag :sacla)
 (assert-true (integerp (read-from-string "48148148031244413808971345."))))
(define-test sacla-must-reader.180 (:tag :sacla)
 (assert-true (integerp (read-from-string "+48148148031244413808971345."))))
(define-test sacla-must-reader.181 (:tag :sacla)
 (assert-true (integerp (read-from-string "-48148148031244413808971345."))))
(define-test sacla-must-reader.182 (:tag :sacla)
 (assert-true
  (zerop
   (let ((*read-base* 2))
     (read-from-string "0")))))
(define-test sacla-must-reader.183 (:tag :sacla)
 (assert-true
  (zerop
   (let ((*read-base* 2))
     (read-from-string "+0")))))
(define-test sacla-must-reader.184 (:tag :sacla)
 (assert-true
  (zerop
   (let ((*read-base* 2))
     (read-from-string "-0")))))
(define-test sacla-must-reader.185 (:tag :sacla)
 (assert-true
  (= 1
     (let ((*read-base* 2))
       (read-from-string "1")))))
(define-test sacla-must-reader.186 (:tag :sacla)
 (assert-true
  (= 1
     (let ((*read-base* 2))
       (read-from-string "+1")))))
(define-test sacla-must-reader.187 (:tag :sacla)
 (assert-true
  (= -1
     (let ((*read-base* 2))
       (read-from-string "-1")))))
(define-test sacla-must-reader.188 (:tag :sacla)
 (assert-true
  (= 2
     (let ((*read-base* 2))
       (read-from-string "10")))))
(define-test sacla-must-reader.189 (:tag :sacla)
 (assert-true
  (= 2
     (let ((*read-base* 2))
       (read-from-string "+10")))))
(define-test sacla-must-reader.190 (:tag :sacla)
 (assert-true
  (= -2
     (let ((*read-base* 2))
       (read-from-string "-10")))))
(define-test sacla-must-reader.191 (:tag :sacla)
 (assert-true
  (= 3
     (let ((*read-base* 2))
       (read-from-string "11")))))
(define-test sacla-must-reader.192 (:tag :sacla)
 (assert-true
  (= 3
     (let ((*read-base* 2))
       (read-from-string "+11")))))
(define-test sacla-must-reader.193 (:tag :sacla)
 (assert-true
  (= -3
     (let ((*read-base* 2))
       (read-from-string "-11")))))
(define-test sacla-must-reader.194 (:tag :sacla)
 (assert-true
  (= -11
     (let ((*read-base* 2))
       (read-from-string "-11.")))))
(define-test sacla-must-reader.195 (:tag :sacla)
 (assert-true
  (integerp
   (let ((*read-base* 2))
     (read-from-string "-11.")))))
(define-test sacla-must-reader.196 (:tag :sacla)
 (assert-true
  (= 21
     (let ((*read-base* 2))
       (read-from-string "10101")))))
(define-test sacla-must-reader.197 (:tag :sacla)
 (assert-true
  (= 21
     (let ((*read-base* 2))
       (read-from-string "+10101")))))
(define-test sacla-must-reader.198 (:tag :sacla)
 (assert-true
  (= -21
     (let ((*read-base* 2))
       (read-from-string "-10101")))))
(define-test sacla-must-reader.199 (:tag :sacla)
 (assert-true
  (= -1.0101
     (let ((*read-base* 2))
       (read-from-string "-1.0101")))))
(define-test sacla-must-reader.200 (:tag :sacla)
 (assert-true
  (= 1.0101
     (let ((*read-base* 2))
       (read-from-string "1.0101")))))
(define-test sacla-must-reader.201 (:tag :sacla)
 (assert-true
  (= 123
     (let ((*read-base* 2))
       (read-from-string "123.")))))
(define-test sacla-must-reader.202 (:tag :sacla)
 (assert-true
  (zerop
   (let ((*read-base* 3))
     (read-from-string "0")))))
(define-test sacla-must-reader.203 (:tag :sacla)
 (assert-true
  (zerop
   (let ((*read-base* 3))
     (read-from-string "+0")))))
(define-test sacla-must-reader.204 (:tag :sacla)
 (assert-true
  (zerop
   (let ((*read-base* 3))
     (read-from-string "-0")))))
(define-test sacla-must-reader.205 (:tag :sacla)
 (assert-true
  (= 1
     (let ((*read-base* 3))
       (read-from-string "1")))))
(define-test sacla-must-reader.206 (:tag :sacla)
 (assert-true
  (= 1
     (let ((*read-base* 3))
       (read-from-string "+1")))))
(define-test sacla-must-reader.207 (:tag :sacla)
 (assert-true
  (= -1
     (let ((*read-base* 3))
       (read-from-string "-1")))))
(define-test sacla-must-reader.208 (:tag :sacla)
 (assert-true
  (= 2
     (let ((*read-base* 3))
       (read-from-string "2")))))
(define-test sacla-must-reader.209 (:tag :sacla)
 (assert-true
  (= 2
     (let ((*read-base* 3))
       (read-from-string "+2")))))
(define-test sacla-must-reader.210 (:tag :sacla)
 (assert-true
  (= -2
     (let ((*read-base* 3))
       (read-from-string "-2")))))
(define-test sacla-must-reader.211 (:tag :sacla)
 (assert-true
  (= 3
     (let ((*read-base* 3))
       (read-from-string "10")))))
(define-test sacla-must-reader.212 (:tag :sacla)
 (assert-true
  (= 3
     (let ((*read-base* 3))
       (read-from-string "+10")))))
(define-test sacla-must-reader.213 (:tag :sacla)
 (assert-true
  (= -3
     (let ((*read-base* 3))
       (read-from-string "-10")))))
(define-test sacla-must-reader.214 (:tag :sacla)
 (assert-true
  (= 4
     (let ((*read-base* 3))
       (read-from-string "11")))))
(define-test sacla-must-reader.215 (:tag :sacla)
 (assert-true
  (= 4
     (let ((*read-base* 3))
       (read-from-string "+11")))))
(define-test sacla-must-reader.216 (:tag :sacla)
 (assert-true
  (= -4
     (let ((*read-base* 3))
       (read-from-string "-11")))))
(define-test sacla-must-reader.217 (:tag :sacla)
 (assert-true
  (= 5
     (let ((*read-base* 3))
       (read-from-string "12")))))
(define-test sacla-must-reader.218 (:tag :sacla)
 (assert-true
  (= 5
     (let ((*read-base* 3))
       (read-from-string "+12")))))
(define-test sacla-must-reader.219 (:tag :sacla)
 (assert-true
  (= -5
     (let ((*read-base* 3))
       (read-from-string "-12")))))
(define-test sacla-must-reader.220 (:tag :sacla)
 (assert-true
  (= 6
     (let ((*read-base* 3))
       (read-from-string "20")))))
(define-test sacla-must-reader.221 (:tag :sacla)
 (assert-true
  (= 6
     (let ((*read-base* 3))
       (read-from-string "+20")))))
(define-test sacla-must-reader.222 (:tag :sacla)
 (assert-true
  (= -6
     (let ((*read-base* 3))
       (read-from-string "-20")))))
(define-test sacla-must-reader.223 (:tag :sacla)
 (assert-true
  (= 7
     (let ((*read-base* 3))
       (read-from-string "21")))))
(define-test sacla-must-reader.224 (:tag :sacla)
 (assert-true
  (= 7
     (let ((*read-base* 3))
       (read-from-string "+21")))))
(define-test sacla-must-reader.225 (:tag :sacla)
 (assert-true
  (= -7
     (let ((*read-base* 3))
       (read-from-string "-21")))))
(define-test sacla-must-reader.226 (:tag :sacla)
 (assert-true
  (= 8
     (let ((*read-base* 3))
       (read-from-string "22")))))
(define-test sacla-must-reader.227 (:tag :sacla)
 (assert-true
  (= 8
     (let ((*read-base* 3))
       (read-from-string "+22")))))
(define-test sacla-must-reader.228 (:tag :sacla)
 (assert-true
  (= -8
     (let ((*read-base* 3))
       (read-from-string "-22")))))
(define-test sacla-must-reader.229 (:tag :sacla)
 (assert-true
  (= 391514
     (let ((*read-base* 3))
       (read-from-string "201220001112")))))
(define-test sacla-must-reader.230 (:tag :sacla)
 (assert-true
  (= 391514
     (let ((*read-base* 3))
       (read-from-string "+201220001112")))))
(define-test sacla-must-reader.231 (:tag :sacla)
 (assert-true
  (= -391514
     (let ((*read-base* 3))
       (read-from-string "-201220001112")))))
(define-test sacla-must-reader.232 (:tag :sacla)
 (assert-true
  (zerop
   (let ((*read-base* 8))
     (read-from-string "0")))))
(define-test sacla-must-reader.233 (:tag :sacla)
 (assert-true
  (zerop
   (let ((*read-base* 8))
     (read-from-string "+0")))))
(define-test sacla-must-reader.234 (:tag :sacla)
 (assert-true
  (zerop
   (let ((*read-base* 8))
     (read-from-string "-0")))))
(define-test sacla-must-reader.235 (:tag :sacla)
 (assert-true
  (= 1
     (let ((*read-base* 8))
       (read-from-string "1")))))
(define-test sacla-must-reader.236 (:tag :sacla)
 (assert-true
  (= 1
     (let ((*read-base* 8))
       (read-from-string "+1")))))
(define-test sacla-must-reader.237 (:tag :sacla)
 (assert-true
  (= -1
     (let ((*read-base* 8))
       (read-from-string "-1")))))
(define-test sacla-must-reader.238 (:tag :sacla)
 (assert-true
  (= 7
     (let ((*read-base* 8))
       (read-from-string "7")))))
(define-test sacla-must-reader.239 (:tag :sacla)
 (assert-true
  (= 7
     (let ((*read-base* 8))
       (read-from-string "+7")))))
(define-test sacla-must-reader.240 (:tag :sacla)
 (assert-true
  (= -7
     (let ((*read-base* 8))
       (read-from-string "-7")))))
(define-test sacla-must-reader.241 (:tag :sacla)
 (assert-true
  (zerop
   (let ((*read-base* 16))
     (read-from-string "0")))))
(define-test sacla-must-reader.242 (:tag :sacla)
 (assert-true
  (zerop
   (let ((*read-base* 16))
     (read-from-string "+0")))))
(define-test sacla-must-reader.243 (:tag :sacla)
 (assert-true
  (zerop
   (let ((*read-base* 16))
     (read-from-string "-0")))))
(define-test sacla-must-reader.244 (:tag :sacla)
 (assert-true
  (= 1
     (let ((*read-base* 16))
       (read-from-string "1")))))
(define-test sacla-must-reader.245 (:tag :sacla)
 (assert-true
  (= 1
     (let ((*read-base* 16))
       (read-from-string "+1")))))
(define-test sacla-must-reader.246 (:tag :sacla)
 (assert-true
  (= -1
     (let ((*read-base* 16))
       (read-from-string "-1")))))
(define-test sacla-must-reader.247 (:tag :sacla)
 (assert-true
  (= 9
     (let ((*read-base* 16))
       (read-from-string "9")))))
(define-test sacla-must-reader.248 (:tag :sacla)
 (assert-true
  (= 9
     (let ((*read-base* 16))
       (read-from-string "+9")))))
(define-test sacla-must-reader.249 (:tag :sacla)
 (assert-true
  (= -9
     (let ((*read-base* 16))
       (read-from-string "-9")))))
(define-test sacla-must-reader.250 (:tag :sacla)
 (assert-true
  (= 15
     (let ((*read-base* 16))
       (read-from-string "F")))))
(define-test sacla-must-reader.251 (:tag :sacla)
 (assert-true
  (= -15
     (let ((*read-base* 16))
       (read-from-string "-F")))))
(define-test sacla-must-reader.252 (:tag :sacla)
 (assert-true
  (= 15
     (let ((*read-base* 16))
       (read-from-string "F")))))
(define-test sacla-must-reader.253 (:tag :sacla)
 (assert-true
  (= 15
     (let ((*read-base* 16))
       (read-from-string "f")))))
(define-test sacla-must-reader.254 (:tag :sacla)
 (assert-true
  (= -15
     (let ((*read-base* 16))
       (read-from-string "-f")))))
(define-test sacla-must-reader.255 (:tag :sacla)
 (assert-true
  (= 15
     (let ((*read-base* 16))
       (read-from-string "f")))))
(define-test sacla-must-reader.256 (:tag :sacla)
 (assert-true
  (= 31
     (let ((*read-base* 16))
       (read-from-string "1F")))))
(define-test sacla-must-reader.257 (:tag :sacla)
 (assert-true
  (= 31
     (let ((*read-base* 16))
       (read-from-string "+1F")))))
(define-test sacla-must-reader.258 (:tag :sacla)
 (assert-true
  (= -31
     (let ((*read-base* 16))
       (read-from-string "-1F")))))
(define-test sacla-must-reader.259 (:tag :sacla)
 (assert-true
  (= 63
     (let ((*read-base* 16))
       (read-from-string "3F")))))
(define-test sacla-must-reader.260 (:tag :sacla)
 (assert-true
  (= 63
     (let ((*read-base* 16))
       (read-from-string "+3F")))))
(define-test sacla-must-reader.261 (:tag :sacla)
 (assert-true
  (= -63
     (let ((*read-base* 16))
       (read-from-string "-3F")))))
(define-test sacla-must-reader.262 (:tag :sacla)
 (assert-true
  (= 9
     (let ((*read-base* 16))
       (read-from-string "9.")))))
(define-test sacla-must-reader.263 (:tag :sacla)
 (assert-true
  (integerp
   (let ((*read-base* 16))
     (read-from-string "9.")))))
(define-test sacla-must-reader.264 (:tag :sacla)
 (assert-true
  (= 10
     (let ((*read-base* 16))
       (read-from-string "10.")))))
(define-test sacla-must-reader.265 (:tag :sacla)
 (assert-true
  (integerp
   (let ((*read-base* 16))
     (read-from-string "10.")))))
(define-test sacla-must-reader.266 (:tag :sacla)
 (assert-true
  (equal
   (let (stack)
     (dotimes (i 6 stack)
       (let ((*read-base* (+ 10 i)))
         (let ((object (read-from-string "(\\DAD DAD |BEE| BEE 123. 123)")))
           (push (list *read-base* object) stack)))))
   '((15 (dad 3088 bee 2699 123 258)) (14 (dad 2701 bee bee 123 227))
     (13 (dad dad bee bee 123 198)) (12 (dad dad bee bee 123 171))
     (11 (dad dad bee bee 123 146)) (10 (dad dad bee bee 123 123))))))
(define-test sacla-must-reader.267 (:tag :sacla)
 (assert-true
  (loop for i from 2 upto 32
        always (zerop
                (let ((*read-base* i))
                  (read-from-string "0"))))))
(define-test sacla-must-reader.268 (:tag :sacla)
 (assert-true
  (loop for i from 2 upto 32
        always (zerop
                (let ((*read-base* i))
                  (read-from-string "+0"))))))
(define-test sacla-must-reader.269 (:tag :sacla)
 (assert-true
  (loop for i from 2 upto 32
        always (zerop
                (let ((*read-base* i))
                  (read-from-string "-0"))))))
(define-test sacla-must-reader.270 (:tag :sacla)
 (assert-true
  (loop for i from 2 upto 32
        always (= 1
                  (let ((*read-base* i))
                    (read-from-string "1"))))))
(define-test sacla-must-reader.271 (:tag :sacla)
 (assert-true
  (loop for i from 2 upto 32
        always (= 1
                  (let ((*read-base* i))
                    (read-from-string "+1"))))))
(define-test sacla-must-reader.272 (:tag :sacla)
 (assert-true
  (loop for i from 2 upto 32
        always (= -1
                  (let ((*read-base* i))
                    (read-from-string "-1"))))))
(define-test sacla-must-reader.273 (:tag :sacla)
 (assert-true
  (loop for i from 2 upto 32
        for n =
            (let ((*read-base* i))
              (read-from-string "10."))
        always (and (integerp n) (= 10 n)))))
(define-test sacla-must-reader.274 (:tag :sacla)
 (assert-true
  (loop for i from 2 upto 32
        for n =
            (let ((*read-base* i))
              (read-from-string "+10."))
        always (and (integerp n) (= 10 n)))))
(define-test sacla-must-reader.275 (:tag :sacla)
 (assert-true
  (loop for i from 2 upto 32
        for n =
            (let ((*read-base* i))
              (read-from-string "-10."))
        always (and (integerp n) (= -10 n)))))
(define-test sacla-must-reader.276 (:tag :sacla)
 (assert-true
  (loop for i from 2 upto 32
        for n =
            (let ((*read-base* i))
              (read-from-string "1.1"))
        always (= 1.1 n))))
(define-test sacla-must-reader.277 (:tag :sacla)
 (assert-true
  (loop for i from 2 upto 32
        for n =
            (let ((*read-base* i))
              (read-from-string "+1.1"))
        always (= 1.1 n))))
(define-test sacla-must-reader.278 (:tag :sacla)
 (assert-true
  (loop for i from 2 upto 32
        for n =
            (let ((*read-base* i))
              (read-from-string "-1.1"))
        always (= -1.1 n))))
(define-test sacla-must-reader.279 (:tag :sacla)
 (assert-true (zerop (read-from-string "0/2"))))
(define-test sacla-must-reader.280 (:tag :sacla)
 (assert-true (zerop (read-from-string "0/3"))))
(define-test sacla-must-reader.281 (:tag :sacla)
 (assert-true (zerop (read-from-string "0/4"))))
(define-test sacla-must-reader.282 (:tag :sacla)
 (assert-true (zerop (read-from-string "0/5"))))
(define-test sacla-must-reader.283 (:tag :sacla)
 (assert-true (zerop (read-from-string "0/6"))))
(define-test sacla-must-reader.284 (:tag :sacla)
 (assert-true (zerop (read-from-string "0/7"))))
(define-test sacla-must-reader.285 (:tag :sacla)
 (assert-true (zerop (read-from-string "0/8"))))
(define-test sacla-must-reader.286 (:tag :sacla)
 (assert-true (zerop (read-from-string "0/9"))))
(define-test sacla-must-reader.287 (:tag :sacla)
 (assert-true (zerop (read-from-string "0/10"))))
(define-test sacla-must-reader.288 (:tag :sacla)
 (assert-true (zerop (read-from-string "0/11"))))
(define-test sacla-must-reader.289 (:tag :sacla)
 (assert-true (zerop (read-from-string "0/12"))))
(define-test sacla-must-reader.290 (:tag :sacla)
 (assert-true (zerop (read-from-string "0/13"))))
(define-test sacla-must-reader.291 (:tag :sacla)
 (assert-true (zerop (read-from-string "0/14"))))
(define-test sacla-must-reader.292 (:tag :sacla)
 (assert-true (zerop (read-from-string "0/15"))))
(define-test sacla-must-reader.293 (:tag :sacla)
 (assert-true (zerop (read-from-string "0/16"))))
(define-test sacla-must-reader.294 (:tag :sacla)
 (assert-true (zerop (read-from-string "0/17"))))
(define-test sacla-must-reader.295 (:tag :sacla)
 (assert-true (zerop (read-from-string "0/18"))))
(define-test sacla-must-reader.296 (:tag :sacla)
 (assert-true (zerop (read-from-string "0/19"))))
(define-test sacla-must-reader.297 (:tag :sacla)
 (assert-true (zerop (read-from-string "0/20"))))
(define-test sacla-must-reader.298 (:tag :sacla)
 (assert-true (= 1/2 (read-from-string "1/2"))))
(define-test sacla-must-reader.299 (:tag :sacla)
 (assert-true (= 1/3 (read-from-string "1/3"))))
(define-test sacla-must-reader.300 (:tag :sacla)
 (assert-true (= 1/4 (read-from-string "1/4"))))
(define-test sacla-must-reader.301 (:tag :sacla)
 (assert-true (= 1/5 (read-from-string "1/5"))))
(define-test sacla-must-reader.302 (:tag :sacla)
 (assert-true (= 1/6 (read-from-string "1/6"))))
(define-test sacla-must-reader.303 (:tag :sacla)
 (assert-true (= 1/7 (read-from-string "1/7"))))
(define-test sacla-must-reader.304 (:tag :sacla)
 (assert-true (= 1/8 (read-from-string "1/8"))))
(define-test sacla-must-reader.305 (:tag :sacla)
 (assert-true (= 1/9 (read-from-string "1/9"))))
(define-test sacla-must-reader.306 (:tag :sacla)
 (assert-true (= 1/10 (read-from-string "1/10"))))
(define-test sacla-must-reader.307 (:tag :sacla)
 (assert-true (= 1/11 (read-from-string "1/11"))))
(define-test sacla-must-reader.308 (:tag :sacla)
 (assert-true (= 1/12 (read-from-string "1/12"))))
(define-test sacla-must-reader.309 (:tag :sacla)
 (assert-true (= 1/13 (read-from-string "1/13"))))
(define-test sacla-must-reader.310 (:tag :sacla)
 (assert-true (= 1/14 (read-from-string "1/14"))))
(define-test sacla-must-reader.311 (:tag :sacla)
 (assert-true (= 1/15 (read-from-string "1/15"))))
(define-test sacla-must-reader.312 (:tag :sacla)
 (assert-true (= 1/16 (read-from-string "1/16"))))
(define-test sacla-must-reader.313 (:tag :sacla)
 (assert-true (= 1/17 (read-from-string "1/17"))))
(define-test sacla-must-reader.314 (:tag :sacla)
 (assert-true (= 1/18 (read-from-string "1/18"))))
(define-test sacla-must-reader.315 (:tag :sacla)
 (assert-true (= 1/19 (read-from-string "1/19"))))
(define-test sacla-must-reader.316 (:tag :sacla)
 (assert-true (= 1/20 (read-from-string "1/20"))))
(define-test sacla-must-reader.317 (:tag :sacla)
 (assert-true (= 1 (read-from-string "2/2"))))
(define-test sacla-must-reader.318 (:tag :sacla)
 (assert-true (= 2/3 (read-from-string "2/3"))))
(define-test sacla-must-reader.319 (:tag :sacla)
 (assert-true (= 1/2 (read-from-string "2/4"))))
(define-test sacla-must-reader.320 (:tag :sacla)
 (assert-true (= 2/5 (read-from-string "2/5"))))
(define-test sacla-must-reader.321 (:tag :sacla)
 (assert-true (= 1/3 (read-from-string "2/6"))))
(define-test sacla-must-reader.322 (:tag :sacla)
 (assert-true (= 2/7 (read-from-string "2/7"))))
(define-test sacla-must-reader.323 (:tag :sacla)
 (assert-true (= 1/4 (read-from-string "2/8"))))
(define-test sacla-must-reader.324 (:tag :sacla)
 (assert-true (= 2/9 (read-from-string "2/9"))))
(define-test sacla-must-reader.325 (:tag :sacla)
 (assert-true (= 1/5 (read-from-string "2/10"))))
(define-test sacla-must-reader.326 (:tag :sacla)
 (assert-true (= 2/11 (read-from-string "2/11"))))
(define-test sacla-must-reader.327 (:tag :sacla)
 (assert-true (= 1/6 (read-from-string "2/12"))))
(define-test sacla-must-reader.328 (:tag :sacla)
 (assert-true (= 2/13 (read-from-string "2/13"))))
(define-test sacla-must-reader.329 (:tag :sacla)
 (assert-true (= 1/7 (read-from-string "2/14"))))
(define-test sacla-must-reader.330 (:tag :sacla)
 (assert-true (= 2/15 (read-from-string "2/15"))))
(define-test sacla-must-reader.331 (:tag :sacla)
 (assert-true (= 1/8 (read-from-string "2/16"))))
(define-test sacla-must-reader.332 (:tag :sacla)
 (assert-true (= 2/17 (read-from-string "2/17"))))
(define-test sacla-must-reader.333 (:tag :sacla)
 (assert-true (= 1/9 (read-from-string "2/18"))))
(define-test sacla-must-reader.334 (:tag :sacla)
 (assert-true (= 2/19 (read-from-string "2/19"))))
(define-test sacla-must-reader.335 (:tag :sacla)
 (assert-true (= 1/10 (read-from-string "2/20"))))
(define-test sacla-must-reader.336 (:tag :sacla)
 (assert-true (= 17/2 (read-from-string "17/2"))))
(define-test sacla-must-reader.337 (:tag :sacla)
 (assert-true (= 17/3 (read-from-string "17/3"))))
(define-test sacla-must-reader.338 (:tag :sacla)
 (assert-true (= 17/4 (read-from-string "17/4"))))
(define-test sacla-must-reader.339 (:tag :sacla)
 (assert-true (= 17/5 (read-from-string "17/5"))))
(define-test sacla-must-reader.340 (:tag :sacla)
 (assert-true (= 17/6 (read-from-string "17/6"))))
(define-test sacla-must-reader.341 (:tag :sacla)
 (assert-true (= 17/7 (read-from-string "17/7"))))
(define-test sacla-must-reader.342 (:tag :sacla)
 (assert-true (= 17/8 (read-from-string "17/8"))))
(define-test sacla-must-reader.343 (:tag :sacla)
 (assert-true (= 17/9 (read-from-string "17/9"))))
(define-test sacla-must-reader.344 (:tag :sacla)
 (assert-true (= 17/10 (read-from-string "17/10"))))
(define-test sacla-must-reader.345 (:tag :sacla)
 (assert-true (= 17/11 (read-from-string "17/11"))))
(define-test sacla-must-reader.346 (:tag :sacla)
 (assert-true (= 17/12 (read-from-string "17/12"))))
(define-test sacla-must-reader.347 (:tag :sacla)
 (assert-true (= 17/13 (read-from-string "17/13"))))
(define-test sacla-must-reader.348 (:tag :sacla)
 (assert-true (= 17/14 (read-from-string "17/14"))))
(define-test sacla-must-reader.349 (:tag :sacla)
 (assert-true (= 17/15 (read-from-string "17/15"))))
(define-test sacla-must-reader.350 (:tag :sacla)
 (assert-true (= 17/16 (read-from-string "17/16"))))
(define-test sacla-must-reader.351 (:tag :sacla)
 (assert-true (= 1 (read-from-string "17/17"))))
(define-test sacla-must-reader.352 (:tag :sacla)
 (assert-true (= 17/18 (read-from-string "17/18"))))
(define-test sacla-must-reader.353 (:tag :sacla)
 (assert-true (= 17/19 (read-from-string "17/19"))))
(define-test sacla-must-reader.354 (:tag :sacla)
 (assert-true (= 17/20 (read-from-string "17/20"))))
(define-test sacla-must-reader.355 (:tag :sacla)
 (assert-true
  (= 0
     (let ((*read-base* 2))
       (read-from-string "0/1")))))
(define-test sacla-must-reader.356 (:tag :sacla)
 (assert-true
  (= 1
     (let ((*read-base* 2))
       (read-from-string "1/1")))))
(define-test sacla-must-reader.357 (:tag :sacla)
 (assert-true
  (= 1/2
     (let ((*read-base* 2))
       (read-from-string "1/10")))))
(define-test sacla-must-reader.358 (:tag :sacla)
 (assert-true
  (= 1/3
     (let ((*read-base* 2))
       (read-from-string "1/11")))))
(define-test sacla-must-reader.359 (:tag :sacla)
 (assert-true
  (= 1/4
     (let ((*read-base* 2))
       (read-from-string "1/100")))))
(define-test sacla-must-reader.360 (:tag :sacla)
 (assert-true
  (= 1/5
     (let ((*read-base* 2))
       (read-from-string "1/101")))))
(define-test sacla-must-reader.361 (:tag :sacla)
 (assert-true
  (= 1/6
     (let ((*read-base* 2))
       (read-from-string "1/110")))))
(define-test sacla-must-reader.362 (:tag :sacla)
 (assert-true
  (= 1/7
     (let ((*read-base* 2))
       (read-from-string "1/111")))))
(define-test sacla-must-reader.363 (:tag :sacla)
 (assert-true
  (= 1/8
     (let ((*read-base* 2))
       (read-from-string "1/1000")))))
(define-test sacla-must-reader.364 (:tag :sacla)
 (assert-true
  (= 1/9
     (let ((*read-base* 2))
       (read-from-string "1/1001")))))
(define-test sacla-must-reader.365 (:tag :sacla)
 (assert-true
  (= 1/10
     (let ((*read-base* 2))
       (read-from-string "1/1010")))))
(define-test sacla-must-reader.366 (:tag :sacla)
 (assert-true
  (= 1/11
     (let ((*read-base* 2))
       (read-from-string "1/1011")))))
(define-test sacla-must-reader.367 (:tag :sacla)
 (assert-true
  (= 1/12
     (let ((*read-base* 2))
       (read-from-string "1/1100")))))
(define-test sacla-must-reader.368 (:tag :sacla)
 (assert-true
  (= 1/13
     (let ((*read-base* 2))
       (read-from-string "1/1101")))))
(define-test sacla-must-reader.369 (:tag :sacla)
 (assert-true
  (= 1/14
     (let ((*read-base* 2))
       (read-from-string "1/1110")))))
(define-test sacla-must-reader.370 (:tag :sacla)
 (assert-true
  (= 1/15
     (let ((*read-base* 2))
       (read-from-string "1/1111")))))
(define-test sacla-must-reader.371 (:tag :sacla)
 (assert-true
  (= 1/16
     (let ((*read-base* 2))
       (read-from-string "1/10000")))))
(define-test sacla-must-reader.372 (:tag :sacla)
 (assert-true
  (= 1/17
     (let ((*read-base* 2))
       (read-from-string "1/10001")))))
(define-test sacla-must-reader.373 (:tag :sacla)
 (assert-true
  (= 1/18
     (let ((*read-base* 2))
       (read-from-string "1/10010")))))
(define-test sacla-must-reader.374 (:tag :sacla)
 (assert-true
  (= 1/19
     (let ((*read-base* 2))
       (read-from-string "1/10011")))))
(define-test sacla-must-reader.375 (:tag :sacla)
 (assert-true
  (= 1/20
     (let ((*read-base* 2))
       (read-from-string "1/10100")))))
(define-test sacla-must-reader.376 (:tag :sacla)
 (assert-true
  (= 1/21
     (let ((*read-base* 2))
       (read-from-string "1/10101")))))
(define-test sacla-must-reader.377 (:tag :sacla)
 (assert-true
  (= 1/22
     (let ((*read-base* 2))
       (read-from-string "1/10110")))))
(define-test sacla-must-reader.378 (:tag :sacla)
 (assert-true
  (= 1/23
     (let ((*read-base* 2))
       (read-from-string "1/10111")))))
(define-test sacla-must-reader.379 (:tag :sacla)
 (assert-true
  (= 2
     (let ((*read-base* 2))
       (read-from-string "10/1")))))
(define-test sacla-must-reader.380 (:tag :sacla)
 (assert-true
  (= 1
     (let ((*read-base* 2))
       (read-from-string "10/10")))))
(define-test sacla-must-reader.381 (:tag :sacla)
 (assert-true
  (= 2/3
     (let ((*read-base* 2))
       (read-from-string "10/11")))))
(define-test sacla-must-reader.382 (:tag :sacla)
 (assert-true
  (= 1/2
     (let ((*read-base* 2))
       (read-from-string "10/100")))))
(define-test sacla-must-reader.383 (:tag :sacla)
 (assert-true
  (= 2/5
     (let ((*read-base* 2))
       (read-from-string "10/101")))))
(define-test sacla-must-reader.384 (:tag :sacla)
 (assert-true
  (= 1/3
     (let ((*read-base* 2))
       (read-from-string "10/110")))))
(define-test sacla-must-reader.385 (:tag :sacla)
 (assert-true
  (= 2/7
     (let ((*read-base* 2))
       (read-from-string "10/111")))))
(define-test sacla-must-reader.386 (:tag :sacla)
 (assert-true
  (= 1/4
     (let ((*read-base* 2))
       (read-from-string "10/1000")))))
(define-test sacla-must-reader.387 (:tag :sacla)
 (assert-true
  (= 2/9
     (let ((*read-base* 2))
       (read-from-string "10/1001")))))
(define-test sacla-must-reader.388 (:tag :sacla)
 (assert-true
  (= 1/5
     (let ((*read-base* 2))
       (read-from-string "10/1010")))))
(define-test sacla-must-reader.389 (:tag :sacla)
 (assert-true
  (= 2/11
     (let ((*read-base* 2))
       (read-from-string "10/1011")))))
(define-test sacla-must-reader.390 (:tag :sacla)
 (assert-true
  (= 1/6
     (let ((*read-base* 2))
       (read-from-string "10/1100")))))
(define-test sacla-must-reader.391 (:tag :sacla)
 (assert-true
  (= 2/13
     (let ((*read-base* 2))
       (read-from-string "10/1101")))))
(define-test sacla-must-reader.392 (:tag :sacla)
 (assert-true
  (= 1/7
     (let ((*read-base* 2))
       (read-from-string "10/1110")))))
(define-test sacla-must-reader.393 (:tag :sacla)
 (assert-true
  (= 2/15
     (let ((*read-base* 2))
       (read-from-string "10/1111")))))
(define-test sacla-must-reader.394 (:tag :sacla)
 (assert-true
  (= 1/8
     (let ((*read-base* 2))
       (read-from-string "10/10000")))))
(define-test sacla-must-reader.395 (:tag :sacla)
 (assert-true
  (= 2/17
     (let ((*read-base* 2))
       (read-from-string "10/10001")))))
(define-test sacla-must-reader.396 (:tag :sacla)
 (assert-true
  (= 1/9
     (let ((*read-base* 2))
       (read-from-string "10/10010")))))
(define-test sacla-must-reader.397 (:tag :sacla)
 (assert-true
  (= 2/19
     (let ((*read-base* 2))
       (read-from-string "10/10011")))))
(define-test sacla-must-reader.398 (:tag :sacla)
 (assert-true
  (= 1/10
     (let ((*read-base* 2))
       (read-from-string "10/10100")))))
(define-test sacla-must-reader.399 (:tag :sacla)
 (assert-true
  (= 2/21
     (let ((*read-base* 2))
       (read-from-string "10/10101")))))
(define-test sacla-must-reader.400 (:tag :sacla)
 (assert-true
  (= 1/11
     (let ((*read-base* 2))
       (read-from-string "10/10110")))))
(define-test sacla-must-reader.401 (:tag :sacla)
 (assert-true
  (= 2/23
     (let ((*read-base* 2))
       (read-from-string "10/10111")))))
(define-test sacla-must-reader.402 (:tag :sacla)
 (assert-true
  (= 3
     (let ((*read-base* 2))
       (read-from-string "11/1")))))
(define-test sacla-must-reader.403 (:tag :sacla)
 (assert-true
  (= 3/2
     (let ((*read-base* 2))
       (read-from-string "11/10")))))
(define-test sacla-must-reader.404 (:tag :sacla)
 (assert-true
  (= 1
     (let ((*read-base* 2))
       (read-from-string "11/11")))))
(define-test sacla-must-reader.405 (:tag :sacla)
 (assert-true
  (= 3/4
     (let ((*read-base* 2))
       (read-from-string "11/100")))))
(define-test sacla-must-reader.406 (:tag :sacla)
 (assert-true
  (= 3/5
     (let ((*read-base* 2))
       (read-from-string "11/101")))))
(define-test sacla-must-reader.407 (:tag :sacla)
 (assert-true
  (= 1/2
     (let ((*read-base* 2))
       (read-from-string "11/110")))))
(define-test sacla-must-reader.408 (:tag :sacla)
 (assert-true
  (= 3/7
     (let ((*read-base* 2))
       (read-from-string "11/111")))))
(define-test sacla-must-reader.409 (:tag :sacla)
 (assert-true
  (= 3/8
     (let ((*read-base* 2))
       (read-from-string "11/1000")))))
(define-test sacla-must-reader.410 (:tag :sacla)
 (assert-true
  (= 1/3
     (let ((*read-base* 2))
       (read-from-string "11/1001")))))
(define-test sacla-must-reader.411 (:tag :sacla)
 (assert-true
  (= 3/10
     (let ((*read-base* 2))
       (read-from-string "11/1010")))))
(define-test sacla-must-reader.412 (:tag :sacla)
 (assert-true
  (= 3/11
     (let ((*read-base* 2))
       (read-from-string "11/1011")))))
(define-test sacla-must-reader.413 (:tag :sacla)
 (assert-true
  (= 1/4
     (let ((*read-base* 2))
       (read-from-string "11/1100")))))
(define-test sacla-must-reader.414 (:tag :sacla)
 (assert-true
  (= 3/13
     (let ((*read-base* 2))
       (read-from-string "11/1101")))))
(define-test sacla-must-reader.415 (:tag :sacla)
 (assert-true
  (= 3/14
     (let ((*read-base* 2))
       (read-from-string "11/1110")))))
(define-test sacla-must-reader.416 (:tag :sacla)
 (assert-true
  (= 1/5
     (let ((*read-base* 2))
       (read-from-string "11/1111")))))
(define-test sacla-must-reader.417 (:tag :sacla)
 (assert-true
  (= 3/16
     (let ((*read-base* 2))
       (read-from-string "11/10000")))))
(define-test sacla-must-reader.418 (:tag :sacla)
 (assert-true
  (= 3/17
     (let ((*read-base* 2))
       (read-from-string "11/10001")))))
(define-test sacla-must-reader.419 (:tag :sacla)
 (assert-true
  (= 1/6
     (let ((*read-base* 2))
       (read-from-string "11/10010")))))
(define-test sacla-must-reader.420 (:tag :sacla)
 (assert-true
  (= 3/19
     (let ((*read-base* 2))
       (read-from-string "11/10011")))))
(define-test sacla-must-reader.421 (:tag :sacla)
 (assert-true
  (= 3/20
     (let ((*read-base* 2))
       (read-from-string "11/10100")))))
(define-test sacla-must-reader.422 (:tag :sacla)
 (assert-true
  (= 1/7
     (let ((*read-base* 2))
       (read-from-string "11/10101")))))
(define-test sacla-must-reader.423 (:tag :sacla)
 (assert-true
  (= 3/22
     (let ((*read-base* 2))
       (read-from-string "11/10110")))))
(define-test sacla-must-reader.424 (:tag :sacla)
 (assert-true
  (= 3/23
     (let ((*read-base* 2))
       (read-from-string "11/10111")))))
(define-test sacla-must-reader.425 (:tag :sacla)
 (assert-true
  (= 0
     (let ((*read-base* 8))
       (read-from-string "0/1")))))
(define-test sacla-must-reader.426 (:tag :sacla)
 (assert-true
  (= 1/2
     (let ((*read-base* 8))
       (read-from-string "1/2")))))
(define-test sacla-must-reader.427 (:tag :sacla)
 (assert-true
  (= 1/3
     (let ((*read-base* 8))
       (read-from-string "1/3")))))
(define-test sacla-must-reader.428 (:tag :sacla)
 (assert-true
  (= 1/4
     (let ((*read-base* 8))
       (read-from-string "1/4")))))
(define-test sacla-must-reader.429 (:tag :sacla)
 (assert-true
  (= 1/5
     (let ((*read-base* 8))
       (read-from-string "1/5")))))
(define-test sacla-must-reader.430 (:tag :sacla)
 (assert-true
  (= 1/6
     (let ((*read-base* 8))
       (read-from-string "1/6")))))
(define-test sacla-must-reader.431 (:tag :sacla)
 (assert-true
  (= 1/7
     (let ((*read-base* 8))
       (read-from-string "1/7")))))
(define-test sacla-must-reader.432 (:tag :sacla)
 (assert-true
  (= 1/8
     (let ((*read-base* 8))
       (read-from-string "1/10")))))
(define-test sacla-must-reader.433 (:tag :sacla)
 (assert-true
  (= 1/9
     (let ((*read-base* 8))
       (read-from-string "1/11")))))
(define-test sacla-must-reader.434 (:tag :sacla)
 (assert-true
  (= 1/10
     (let ((*read-base* 8))
       (read-from-string "1/12")))))
(define-test sacla-must-reader.435 (:tag :sacla)
 (assert-true
  (= 1/11
     (let ((*read-base* 8))
       (read-from-string "1/13")))))
(define-test sacla-must-reader.436 (:tag :sacla)
 (assert-true
  (= 1/12
     (let ((*read-base* 8))
       (read-from-string "1/14")))))
(define-test sacla-must-reader.437 (:tag :sacla)
 (assert-true
  (= 1/13
     (let ((*read-base* 8))
       (read-from-string "1/15")))))
(define-test sacla-must-reader.438 (:tag :sacla)
 (assert-true
  (= 1/14
     (let ((*read-base* 8))
       (read-from-string "1/16")))))
(define-test sacla-must-reader.439 (:tag :sacla)
 (assert-true
  (= 1/15
     (let ((*read-base* 8))
       (read-from-string "1/17")))))
(define-test sacla-must-reader.440 (:tag :sacla)
 (assert-true
  (= 1/16
     (let ((*read-base* 8))
       (read-from-string "1/20")))))
(define-test sacla-must-reader.441 (:tag :sacla)
 (assert-true
  (= 1/17
     (let ((*read-base* 8))
       (read-from-string "1/21")))))
(define-test sacla-must-reader.442 (:tag :sacla)
 (assert-true
  (= 1/18
     (let ((*read-base* 8))
       (read-from-string "1/22")))))
(define-test sacla-must-reader.443 (:tag :sacla)
 (assert-true
  (= 1/19
     (let ((*read-base* 8))
       (read-from-string "1/23")))))
(define-test sacla-must-reader.444 (:tag :sacla)
 (assert-true
  (= 1/20
     (let ((*read-base* 8))
       (read-from-string "1/24")))))
(define-test sacla-must-reader.445 (:tag :sacla)
 (assert-true
  (= 3/2
     (let ((*read-base* 8))
       (read-from-string "3/2")))))
(define-test sacla-must-reader.446 (:tag :sacla)
 (assert-true
  (= 1
     (let ((*read-base* 8))
       (read-from-string "3/3")))))
(define-test sacla-must-reader.447 (:tag :sacla)
 (assert-true
  (= 3/4
     (let ((*read-base* 8))
       (read-from-string "3/4")))))
(define-test sacla-must-reader.448 (:tag :sacla)
 (assert-true
  (= 3/5
     (let ((*read-base* 8))
       (read-from-string "3/5")))))
(define-test sacla-must-reader.449 (:tag :sacla)
 (assert-true
  (= 1/2
     (let ((*read-base* 8))
       (read-from-string "3/6")))))
(define-test sacla-must-reader.450 (:tag :sacla)
 (assert-true
  (= 3/7
     (let ((*read-base* 8))
       (read-from-string "3/7")))))
(define-test sacla-must-reader.451 (:tag :sacla)
 (assert-true
  (= 3/8
     (let ((*read-base* 8))
       (read-from-string "3/10")))))
(define-test sacla-must-reader.452 (:tag :sacla)
 (assert-true
  (= 1/3
     (let ((*read-base* 8))
       (read-from-string "3/11")))))
(define-test sacla-must-reader.453 (:tag :sacla)
 (assert-true
  (= 3/10
     (let ((*read-base* 8))
       (read-from-string "3/12")))))
(define-test sacla-must-reader.454 (:tag :sacla)
 (assert-true
  (= 3/11
     (let ((*read-base* 8))
       (read-from-string "3/13")))))
(define-test sacla-must-reader.455 (:tag :sacla)
 (assert-true
  (= 1/4
     (let ((*read-base* 8))
       (read-from-string "3/14")))))
(define-test sacla-must-reader.456 (:tag :sacla)
 (assert-true
  (= 3/13
     (let ((*read-base* 8))
       (read-from-string "3/15")))))
(define-test sacla-must-reader.457 (:tag :sacla)
 (assert-true
  (= 3/14
     (let ((*read-base* 8))
       (read-from-string "3/16")))))
(define-test sacla-must-reader.458 (:tag :sacla)
 (assert-true
  (= 1/5
     (let ((*read-base* 8))
       (read-from-string "3/17")))))
(define-test sacla-must-reader.459 (:tag :sacla)
 (assert-true
  (= 3/16
     (let ((*read-base* 8))
       (read-from-string "3/20")))))
(define-test sacla-must-reader.460 (:tag :sacla)
 (assert-true
  (= 3/17
     (let ((*read-base* 8))
       (read-from-string "3/21")))))
(define-test sacla-must-reader.461 (:tag :sacla)
 (assert-true
  (= 1/6
     (let ((*read-base* 8))
       (read-from-string "3/22")))))
(define-test sacla-must-reader.462 (:tag :sacla)
 (assert-true
  (= 3/19
     (let ((*read-base* 8))
       (read-from-string "3/23")))))
(define-test sacla-must-reader.463 (:tag :sacla)
 (assert-true
  (= 3/20
     (let ((*read-base* 8))
       (read-from-string "3/24")))))
(define-test sacla-must-reader.464 (:tag :sacla)
 (assert-true
  (= 13/2
     (let ((*read-base* 8))
       (read-from-string "15/2")))))
(define-test sacla-must-reader.465 (:tag :sacla)
 (assert-true
  (= 13/3
     (let ((*read-base* 8))
       (read-from-string "15/3")))))
(define-test sacla-must-reader.466 (:tag :sacla)
 (assert-true
  (= 13/4
     (let ((*read-base* 8))
       (read-from-string "15/4")))))
(define-test sacla-must-reader.467 (:tag :sacla)
 (assert-true
  (= 13/5
     (let ((*read-base* 8))
       (read-from-string "15/5")))))
(define-test sacla-must-reader.468 (:tag :sacla)
 (assert-true
  (= 13/6
     (let ((*read-base* 8))
       (read-from-string "15/6")))))
(define-test sacla-must-reader.469 (:tag :sacla)
 (assert-true
  (= 13/7
     (let ((*read-base* 8))
       (read-from-string "15/7")))))
(define-test sacla-must-reader.470 (:tag :sacla)
 (assert-true
  (= 13/8
     (let ((*read-base* 8))
       (read-from-string "15/10")))))
(define-test sacla-must-reader.471 (:tag :sacla)
 (assert-true
  (= 13/9
     (let ((*read-base* 8))
       (read-from-string "15/11")))))
(define-test sacla-must-reader.472 (:tag :sacla)
 (assert-true
  (= 13/10
     (let ((*read-base* 8))
       (read-from-string "15/12")))))
(define-test sacla-must-reader.473 (:tag :sacla)
 (assert-true
  (= 13/11
     (let ((*read-base* 8))
       (read-from-string "15/13")))))
(define-test sacla-must-reader.474 (:tag :sacla)
 (assert-true
  (= 13/12
     (let ((*read-base* 8))
       (read-from-string "15/14")))))
(define-test sacla-must-reader.475 (:tag :sacla)
 (assert-true
  (= 1
     (let ((*read-base* 8))
       (read-from-string "15/15")))))
(define-test sacla-must-reader.476 (:tag :sacla)
 (assert-true
  (= 13/14
     (let ((*read-base* 8))
       (read-from-string "15/16")))))
(define-test sacla-must-reader.477 (:tag :sacla)
 (assert-true
  (= 13/15
     (let ((*read-base* 8))
       (read-from-string "15/17")))))
(define-test sacla-must-reader.478 (:tag :sacla)
 (assert-true
  (= 13/16
     (let ((*read-base* 8))
       (read-from-string "15/20")))))
(define-test sacla-must-reader.479 (:tag :sacla)
 (assert-true
  (= 13/17
     (let ((*read-base* 8))
       (read-from-string "15/21")))))
(define-test sacla-must-reader.480 (:tag :sacla)
 (assert-true
  (= 13/18
     (let ((*read-base* 8))
       (read-from-string "15/22")))))
(define-test sacla-must-reader.481 (:tag :sacla)
 (assert-true
  (= 13/19
     (let ((*read-base* 8))
       (read-from-string "15/23")))))
(define-test sacla-must-reader.482 (:tag :sacla)
 (assert-true
  (= 13/20
     (let ((*read-base* 8))
       (read-from-string "15/24")))))
(define-test sacla-must-reader.483 (:tag :sacla)
 (assert-true
  (= 0
     (let ((*read-base* 16))
       (read-from-string "0/1")))))
(define-test sacla-must-reader.484 (:tag :sacla)
 (assert-true
  (= 1/2
     (let ((*read-base* 16))
       (read-from-string "1/2")))))
(define-test sacla-must-reader.485 (:tag :sacla)
 (assert-true
  (= 1/3
     (let ((*read-base* 16))
       (read-from-string "1/3")))))
(define-test sacla-must-reader.486 (:tag :sacla)
 (assert-true
  (= 1/4
     (let ((*read-base* 16))
       (read-from-string "1/4")))))
(define-test sacla-must-reader.487 (:tag :sacla)
 (assert-true
  (= 1/5
     (let ((*read-base* 16))
       (read-from-string "1/5")))))
(define-test sacla-must-reader.488 (:tag :sacla)
 (assert-true
  (= 1/6
     (let ((*read-base* 16))
       (read-from-string "1/6")))))
(define-test sacla-must-reader.489 (:tag :sacla)
 (assert-true
  (= 1/7
     (let ((*read-base* 16))
       (read-from-string "1/7")))))
(define-test sacla-must-reader.490 (:tag :sacla)
 (assert-true
  (= 1/8
     (let ((*read-base* 16))
       (read-from-string "1/8")))))
(define-test sacla-must-reader.491 (:tag :sacla)
 (assert-true
  (= 1/9
     (let ((*read-base* 16))
       (read-from-string "1/9")))))
(define-test sacla-must-reader.492 (:tag :sacla)
 (assert-true
  (= 1/10
     (let ((*read-base* 16))
       (read-from-string "1/A")))))
(define-test sacla-must-reader.493 (:tag :sacla)
 (assert-true
  (= 1/11
     (let ((*read-base* 16))
       (read-from-string "1/B")))))
(define-test sacla-must-reader.494 (:tag :sacla)
 (assert-true
  (= 1/12
     (let ((*read-base* 16))
       (read-from-string "1/C")))))
(define-test sacla-must-reader.495 (:tag :sacla)
 (assert-true
  (= 1/13
     (let ((*read-base* 16))
       (read-from-string "1/D")))))
(define-test sacla-must-reader.496 (:tag :sacla)
 (assert-true
  (= 1/14
     (let ((*read-base* 16))
       (read-from-string "1/E")))))
(define-test sacla-must-reader.497 (:tag :sacla)
 (assert-true
  (= 1/15
     (let ((*read-base* 16))
       (read-from-string "1/F")))))
(define-test sacla-must-reader.498 (:tag :sacla)
 (assert-true
  (= 1/10
     (let ((*read-base* 16))
       (read-from-string "1/a")))))
(define-test sacla-must-reader.499 (:tag :sacla)
 (assert-true
  (= 1/11
     (let ((*read-base* 16))
       (read-from-string "1/b")))))
(define-test sacla-must-reader.500 (:tag :sacla)
 (assert-true
  (= 1/12
     (let ((*read-base* 16))
       (read-from-string "1/c")))))
(define-test sacla-must-reader.501 (:tag :sacla)
 (assert-true
  (= 1/13
     (let ((*read-base* 16))
       (read-from-string "1/d")))))
(define-test sacla-must-reader.502 (:tag :sacla)
 (assert-true
  (= 1/14
     (let ((*read-base* 16))
       (read-from-string "1/e")))))
(define-test sacla-must-reader.503 (:tag :sacla)
 (assert-true
  (= 1/15
     (let ((*read-base* 16))
       (read-from-string "1/f")))))
(define-test sacla-must-reader.504 (:tag :sacla)
 (assert-true
  (= 1/16
     (let ((*read-base* 16))
       (read-from-string "1/10")))))
(define-test sacla-must-reader.505 (:tag :sacla)
 (assert-true
  (= 1/17
     (let ((*read-base* 16))
       (read-from-string "1/11")))))
(define-test sacla-must-reader.506 (:tag :sacla)
 (assert-true
  (= 1/18
     (let ((*read-base* 16))
       (read-from-string "1/12")))))
(define-test sacla-must-reader.507 (:tag :sacla)
 (assert-true
  (= 1/19
     (let ((*read-base* 16))
       (read-from-string "1/13")))))
(define-test sacla-must-reader.508 (:tag :sacla)
 (assert-true
  (= 1/20
     (let ((*read-base* 16))
       (read-from-string "1/14")))))
(define-test sacla-must-reader.509 (:tag :sacla)
 (assert-true
  (= 1/21
     (let ((*read-base* 16))
       (read-from-string "1/15")))))
(define-test sacla-must-reader.510 (:tag :sacla)
 (assert-true
  (= 1/22
     (let ((*read-base* 16))
       (read-from-string "1/16")))))
(define-test sacla-must-reader.511 (:tag :sacla)
 (assert-true
  (= 1/23
     (let ((*read-base* 16))
       (read-from-string "1/17")))))
(define-test sacla-must-reader.512 (:tag :sacla)
 (assert-true
  (= 1/24
     (let ((*read-base* 16))
       (read-from-string "1/18")))))
(define-test sacla-must-reader.513 (:tag :sacla)
 (assert-true
  (= 1/25
     (let ((*read-base* 16))
       (read-from-string "1/19")))))
(define-test sacla-must-reader.514 (:tag :sacla)
 (assert-true
  (= 1/26
     (let ((*read-base* 16))
       (read-from-string "1/1A")))))
(define-test sacla-must-reader.515 (:tag :sacla)
 (assert-true
  (= 1/27
     (let ((*read-base* 16))
       (read-from-string "1/1B")))))
(define-test sacla-must-reader.516 (:tag :sacla)
 (assert-true
  (= 1/28
     (let ((*read-base* 16))
       (read-from-string "1/1C")))))
(define-test sacla-must-reader.517 (:tag :sacla)
 (assert-true
  (= 1/29
     (let ((*read-base* 16))
       (read-from-string "1/1D")))))
(define-test sacla-must-reader.518 (:tag :sacla)
 (assert-true
  (= 1/30
     (let ((*read-base* 16))
       (read-from-string "1/1E")))))
(define-test sacla-must-reader.519 (:tag :sacla)
 (assert-true
  (= 1/31
     (let ((*read-base* 16))
       (read-from-string "1/1F")))))
(define-test sacla-must-reader.520 (:tag :sacla)
 (assert-true
  (= 1/32
     (let ((*read-base* 16))
       (read-from-string "1/20")))))
(define-test sacla-must-reader.521 (:tag :sacla)
 (assert-true
  (= 1/33
     (let ((*read-base* 16))
       (read-from-string "1/21")))))
(define-test sacla-must-reader.522 (:tag :sacla)
 (assert-true
  (= 1/34
     (let ((*read-base* 16))
       (read-from-string "1/22")))))
(define-test sacla-must-reader.523 (:tag :sacla)
 (assert-true
  (= 1/35
     (let ((*read-base* 16))
       (read-from-string "1/23")))))
(define-test sacla-must-reader.524 (:tag :sacla)
 (assert-true
  (= 1/36
     (let ((*read-base* 16))
       (read-from-string "1/24")))))
(define-test sacla-must-reader.525 (:tag :sacla)
 (assert-true
  (= 1
     (let ((*read-base* 16))
       (read-from-string "2/2")))))
(define-test sacla-must-reader.526 (:tag :sacla)
 (assert-true
  (= 2/3
     (let ((*read-base* 16))
       (read-from-string "2/3")))))
(define-test sacla-must-reader.527 (:tag :sacla)
 (assert-true
  (= 1/2
     (let ((*read-base* 16))
       (read-from-string "2/4")))))
(define-test sacla-must-reader.528 (:tag :sacla)
 (assert-true
  (= 2/5
     (let ((*read-base* 16))
       (read-from-string "2/5")))))
(define-test sacla-must-reader.529 (:tag :sacla)
 (assert-true
  (= 1/3
     (let ((*read-base* 16))
       (read-from-string "2/6")))))
(define-test sacla-must-reader.530 (:tag :sacla)
 (assert-true
  (= 2/7
     (let ((*read-base* 16))
       (read-from-string "2/7")))))
(define-test sacla-must-reader.531 (:tag :sacla)
 (assert-true
  (= 1/4
     (let ((*read-base* 16))
       (read-from-string "2/8")))))
(define-test sacla-must-reader.532 (:tag :sacla)
 (assert-true
  (= 2/9
     (let ((*read-base* 16))
       (read-from-string "2/9")))))
(define-test sacla-must-reader.533 (:tag :sacla)
 (assert-true
  (= 1/5
     (let ((*read-base* 16))
       (read-from-string "2/A")))))
(define-test sacla-must-reader.534 (:tag :sacla)
 (assert-true
  (= 2/11
     (let ((*read-base* 16))
       (read-from-string "2/B")))))
(define-test sacla-must-reader.535 (:tag :sacla)
 (assert-true
  (= 1/6
     (let ((*read-base* 16))
       (read-from-string "2/C")))))
(define-test sacla-must-reader.536 (:tag :sacla)
 (assert-true
  (= 2/13
     (let ((*read-base* 16))
       (read-from-string "2/D")))))
(define-test sacla-must-reader.537 (:tag :sacla)
 (assert-true
  (= 1/7
     (let ((*read-base* 16))
       (read-from-string "2/E")))))
(define-test sacla-must-reader.538 (:tag :sacla)
 (assert-true
  (= 2/15
     (let ((*read-base* 16))
       (read-from-string "2/F")))))
(define-test sacla-must-reader.539 (:tag :sacla)
 (assert-true
  (= 1/5
     (let ((*read-base* 16))
       (read-from-string "2/a")))))
(define-test sacla-must-reader.540 (:tag :sacla)
 (assert-true
  (= 2/11
     (let ((*read-base* 16))
       (read-from-string "2/b")))))
(define-test sacla-must-reader.541 (:tag :sacla)
 (assert-true
  (= 1/6
     (let ((*read-base* 16))
       (read-from-string "2/c")))))
(define-test sacla-must-reader.542 (:tag :sacla)
 (assert-true
  (= 2/13
     (let ((*read-base* 16))
       (read-from-string "2/d")))))
(define-test sacla-must-reader.543 (:tag :sacla)
 (assert-true
  (= 1/7
     (let ((*read-base* 16))
       (read-from-string "2/e")))))
(define-test sacla-must-reader.544 (:tag :sacla)
 (assert-true
  (= 2/15
     (let ((*read-base* 16))
       (read-from-string "2/f")))))
(define-test sacla-must-reader.545 (:tag :sacla)
 (assert-true
  (= 1/8
     (let ((*read-base* 16))
       (read-from-string "2/10")))))
(define-test sacla-must-reader.546 (:tag :sacla)
 (assert-true
  (= 2/17
     (let ((*read-base* 16))
       (read-from-string "2/11")))))
(define-test sacla-must-reader.547 (:tag :sacla)
 (assert-true
  (= 1/9
     (let ((*read-base* 16))
       (read-from-string "2/12")))))
(define-test sacla-must-reader.548 (:tag :sacla)
 (assert-true
  (= 2/19
     (let ((*read-base* 16))
       (read-from-string "2/13")))))
(define-test sacla-must-reader.549 (:tag :sacla)
 (assert-true
  (= 1/10
     (let ((*read-base* 16))
       (read-from-string "2/14")))))
(define-test sacla-must-reader.550 (:tag :sacla)
 (assert-true
  (= 2/21
     (let ((*read-base* 16))
       (read-from-string "2/15")))))
(define-test sacla-must-reader.551 (:tag :sacla)
 (assert-true
  (= 1/11
     (let ((*read-base* 16))
       (read-from-string "2/16")))))
(define-test sacla-must-reader.552 (:tag :sacla)
 (assert-true
  (= 2/23
     (let ((*read-base* 16))
       (read-from-string "2/17")))))
(define-test sacla-must-reader.553 (:tag :sacla)
 (assert-true
  (= 1/12
     (let ((*read-base* 16))
       (read-from-string "2/18")))))
(define-test sacla-must-reader.554 (:tag :sacla)
 (assert-true
  (= 2/25
     (let ((*read-base* 16))
       (read-from-string "2/19")))))
(define-test sacla-must-reader.555 (:tag :sacla)
 (assert-true
  (= 1/13
     (let ((*read-base* 16))
       (read-from-string "2/1A")))))
(define-test sacla-must-reader.556 (:tag :sacla)
 (assert-true
  (= 2/27
     (let ((*read-base* 16))
       (read-from-string "2/1B")))))
(define-test sacla-must-reader.557 (:tag :sacla)
 (assert-true
  (= 1/14
     (let ((*read-base* 16))
       (read-from-string "2/1C")))))
(define-test sacla-must-reader.558 (:tag :sacla)
 (assert-true
  (= 2/29
     (let ((*read-base* 16))
       (read-from-string "2/1D")))))
(define-test sacla-must-reader.559 (:tag :sacla)
 (assert-true
  (= 1/15
     (let ((*read-base* 16))
       (read-from-string "2/1E")))))
(define-test sacla-must-reader.560 (:tag :sacla)
 (assert-true
  (= 2/31
     (let ((*read-base* 16))
       (read-from-string "2/1F")))))
(define-test sacla-must-reader.561 (:tag :sacla)
 (assert-true
  (= 1/16
     (let ((*read-base* 16))
       (read-from-string "2/20")))))
(define-test sacla-must-reader.562 (:tag :sacla)
 (assert-true
  (= 2/33
     (let ((*read-base* 16))
       (read-from-string "2/21")))))
(define-test sacla-must-reader.563 (:tag :sacla)
 (assert-true
  (= 1/17
     (let ((*read-base* 16))
       (read-from-string "2/22")))))
(define-test sacla-must-reader.564 (:tag :sacla)
 (assert-true
  (= 2/35
     (let ((*read-base* 16))
       (read-from-string "2/23")))))
(define-test sacla-must-reader.565 (:tag :sacla)
 (assert-true
  (= 1/18
     (let ((*read-base* 16))
       (read-from-string "2/24")))))
(define-test sacla-must-reader.566 (:tag :sacla)
 (assert-true
  (= 5
     (let ((*read-base* 16))
       (read-from-string "a/2")))))
(define-test sacla-must-reader.567 (:tag :sacla)
 (assert-true
  (= 10/3
     (let ((*read-base* 16))
       (read-from-string "a/3")))))
(define-test sacla-must-reader.568 (:tag :sacla)
 (assert-true
  (= 5/2
     (let ((*read-base* 16))
       (read-from-string "a/4")))))
(define-test sacla-must-reader.569 (:tag :sacla)
 (assert-true
  (= 2
     (let ((*read-base* 16))
       (read-from-string "a/5")))))
(define-test sacla-must-reader.570 (:tag :sacla)
 (assert-true
  (= 5/3
     (let ((*read-base* 16))
       (read-from-string "a/6")))))
(define-test sacla-must-reader.571 (:tag :sacla)
 (assert-true
  (= 10/7
     (let ((*read-base* 16))
       (read-from-string "a/7")))))
(define-test sacla-must-reader.572 (:tag :sacla)
 (assert-true
  (= 5/4
     (let ((*read-base* 16))
       (read-from-string "a/8")))))
(define-test sacla-must-reader.573 (:tag :sacla)
 (assert-true
  (= 10/9
     (let ((*read-base* 16))
       (read-from-string "a/9")))))
(define-test sacla-must-reader.574 (:tag :sacla)
 (assert-true
  (= 1
     (let ((*read-base* 16))
       (read-from-string "a/A")))))
(define-test sacla-must-reader.575 (:tag :sacla)
 (assert-true
  (= 10/11
     (let ((*read-base* 16))
       (read-from-string "a/B")))))
(define-test sacla-must-reader.576 (:tag :sacla)
 (assert-true
  (= 5/6
     (let ((*read-base* 16))
       (read-from-string "a/C")))))
(define-test sacla-must-reader.577 (:tag :sacla)
 (assert-true
  (= 10/13
     (let ((*read-base* 16))
       (read-from-string "a/D")))))
(define-test sacla-must-reader.578 (:tag :sacla)
 (assert-true
  (= 5/7
     (let ((*read-base* 16))
       (read-from-string "a/E")))))
(define-test sacla-must-reader.579 (:tag :sacla)
 (assert-true
  (= 2/3
     (let ((*read-base* 16))
       (read-from-string "a/F")))))
(define-test sacla-must-reader.580 (:tag :sacla)
 (assert-true
  (= 1
     (let ((*read-base* 16))
       (read-from-string "a/a")))))
(define-test sacla-must-reader.581 (:tag :sacla)
 (assert-true
  (= 10/11
     (let ((*read-base* 16))
       (read-from-string "a/b")))))
(define-test sacla-must-reader.582 (:tag :sacla)
 (assert-true
  (= 5/6
     (let ((*read-base* 16))
       (read-from-string "a/c")))))
(define-test sacla-must-reader.583 (:tag :sacla)
 (assert-true
  (= 10/13
     (let ((*read-base* 16))
       (read-from-string "a/d")))))
(define-test sacla-must-reader.584 (:tag :sacla)
 (assert-true
  (= 5/7
     (let ((*read-base* 16))
       (read-from-string "a/e")))))
(define-test sacla-must-reader.585 (:tag :sacla)
 (assert-true
  (= 2/3
     (let ((*read-base* 16))
       (read-from-string "a/f")))))
(define-test sacla-must-reader.586 (:tag :sacla)
 (assert-true
  (= 5/8
     (let ((*read-base* 16))
       (read-from-string "a/10")))))
(define-test sacla-must-reader.587 (:tag :sacla)
 (assert-true
  (= 10/17
     (let ((*read-base* 16))
       (read-from-string "a/11")))))
(define-test sacla-must-reader.588 (:tag :sacla)
 (assert-true
  (= 5/9
     (let ((*read-base* 16))
       (read-from-string "a/12")))))
(define-test sacla-must-reader.589 (:tag :sacla)
 (assert-true
  (= 10/19
     (let ((*read-base* 16))
       (read-from-string "a/13")))))
(define-test sacla-must-reader.590 (:tag :sacla)
 (assert-true
  (= 1/2
     (let ((*read-base* 16))
       (read-from-string "a/14")))))
(define-test sacla-must-reader.591 (:tag :sacla)
 (assert-true
  (= 10/21
     (let ((*read-base* 16))
       (read-from-string "a/15")))))
(define-test sacla-must-reader.592 (:tag :sacla)
 (assert-true
  (= 5/11
     (let ((*read-base* 16))
       (read-from-string "a/16")))))
(define-test sacla-must-reader.593 (:tag :sacla)
 (assert-true
  (= 10/23
     (let ((*read-base* 16))
       (read-from-string "a/17")))))
(define-test sacla-must-reader.594 (:tag :sacla)
 (assert-true
  (= 5/12
     (let ((*read-base* 16))
       (read-from-string "a/18")))))
(define-test sacla-must-reader.595 (:tag :sacla)
 (assert-true
  (= 2/5
     (let ((*read-base* 16))
       (read-from-string "a/19")))))
(define-test sacla-must-reader.596 (:tag :sacla)
 (assert-true
  (= 5/13
     (let ((*read-base* 16))
       (read-from-string "a/1A")))))
(define-test sacla-must-reader.597 (:tag :sacla)
 (assert-true
  (= 10/27
     (let ((*read-base* 16))
       (read-from-string "a/1B")))))
(define-test sacla-must-reader.598 (:tag :sacla)
 (assert-true
  (= 5/14
     (let ((*read-base* 16))
       (read-from-string "a/1C")))))
(define-test sacla-must-reader.599 (:tag :sacla)
 (assert-true
  (= 10/29
     (let ((*read-base* 16))
       (read-from-string "a/1D")))))
(define-test sacla-must-reader.600 (:tag :sacla)
 (assert-true
  (= 1/3
     (let ((*read-base* 16))
       (read-from-string "a/1E")))))
(define-test sacla-must-reader.601 (:tag :sacla)
 (assert-true
  (= 10/31
     (let ((*read-base* 16))
       (read-from-string "a/1F")))))
(define-test sacla-must-reader.602 (:tag :sacla)
 (assert-true
  (= 5/16
     (let ((*read-base* 16))
       (read-from-string "a/20")))))
(define-test sacla-must-reader.603 (:tag :sacla)
 (assert-true
  (= 10/33
     (let ((*read-base* 16))
       (read-from-string "a/21")))))
(define-test sacla-must-reader.604 (:tag :sacla)
 (assert-true
  (= 5/17
     (let ((*read-base* 16))
       (read-from-string "a/22")))))
(define-test sacla-must-reader.605 (:tag :sacla)
 (assert-true
  (= 2/7
     (let ((*read-base* 16))
       (read-from-string "a/23")))))
(define-test sacla-must-reader.606 (:tag :sacla)
 (assert-true
  (= 5/18
     (let ((*read-base* 16))
       (read-from-string "a/24")))))
(define-test sacla-must-reader.607 (:tag :sacla)
 (assert-true
  (= 35/2
     (let ((*read-base* 16))
       (read-from-string "23/2")))))
(define-test sacla-must-reader.608 (:tag :sacla)
 (assert-true
  (= 35/3
     (let ((*read-base* 16))
       (read-from-string "23/3")))))
(define-test sacla-must-reader.609 (:tag :sacla)
 (assert-true
  (= 35/4
     (let ((*read-base* 16))
       (read-from-string "23/4")))))
(define-test sacla-must-reader.610 (:tag :sacla)
 (assert-true
  (= 7
     (let ((*read-base* 16))
       (read-from-string "23/5")))))
(define-test sacla-must-reader.611 (:tag :sacla)
 (assert-true
  (= 35/6
     (let ((*read-base* 16))
       (read-from-string "23/6")))))
(define-test sacla-must-reader.612 (:tag :sacla)
 (assert-true
  (= 5
     (let ((*read-base* 16))
       (read-from-string "23/7")))))
(define-test sacla-must-reader.613 (:tag :sacla)
 (assert-true
  (= 35/8
     (let ((*read-base* 16))
       (read-from-string "23/8")))))
(define-test sacla-must-reader.614 (:tag :sacla)
 (assert-true
  (= 35/9
     (let ((*read-base* 16))
       (read-from-string "23/9")))))
(define-test sacla-must-reader.615 (:tag :sacla)
 (assert-true
  (= 7/2
     (let ((*read-base* 16))
       (read-from-string "23/A")))))
(define-test sacla-must-reader.616 (:tag :sacla)
 (assert-true
  (= 35/11
     (let ((*read-base* 16))
       (read-from-string "23/B")))))
(define-test sacla-must-reader.617 (:tag :sacla)
 (assert-true
  (= 35/12
     (let ((*read-base* 16))
       (read-from-string "23/C")))))
(define-test sacla-must-reader.618 (:tag :sacla)
 (assert-true
  (= 35/13
     (let ((*read-base* 16))
       (read-from-string "23/D")))))
(define-test sacla-must-reader.619 (:tag :sacla)
 (assert-true
  (= 5/2
     (let ((*read-base* 16))
       (read-from-string "23/E")))))
(define-test sacla-must-reader.620 (:tag :sacla)
 (assert-true
  (= 7/3
     (let ((*read-base* 16))
       (read-from-string "23/F")))))
(define-test sacla-must-reader.621 (:tag :sacla)
 (assert-true
  (= 7/2
     (let ((*read-base* 16))
       (read-from-string "23/a")))))
(define-test sacla-must-reader.622 (:tag :sacla)
 (assert-true
  (= 35/11
     (let ((*read-base* 16))
       (read-from-string "23/b")))))
(define-test sacla-must-reader.623 (:tag :sacla)
 (assert-true
  (= 35/12
     (let ((*read-base* 16))
       (read-from-string "23/c")))))
(define-test sacla-must-reader.624 (:tag :sacla)
 (assert-true
  (= 35/13
     (let ((*read-base* 16))
       (read-from-string "23/d")))))
(define-test sacla-must-reader.625 (:tag :sacla)
 (assert-true
  (= 5/2
     (let ((*read-base* 16))
       (read-from-string "23/e")))))
(define-test sacla-must-reader.626 (:tag :sacla)
 (assert-true
  (= 7/3
     (let ((*read-base* 16))
       (read-from-string "23/f")))))
(define-test sacla-must-reader.627 (:tag :sacla)
 (assert-true
  (= 35/16
     (let ((*read-base* 16))
       (read-from-string "23/10")))))
(define-test sacla-must-reader.628 (:tag :sacla)
 (assert-true
  (= 35/17
     (let ((*read-base* 16))
       (read-from-string "23/11")))))
(define-test sacla-must-reader.629 (:tag :sacla)
 (assert-true
  (= 35/18
     (let ((*read-base* 16))
       (read-from-string "23/12")))))
(define-test sacla-must-reader.630 (:tag :sacla)
 (assert-true
  (= 35/19
     (let ((*read-base* 16))
       (read-from-string "23/13")))))
(define-test sacla-must-reader.631 (:tag :sacla)
 (assert-true
  (= 7/4
     (let ((*read-base* 16))
       (read-from-string "23/14")))))
(define-test sacla-must-reader.632 (:tag :sacla)
 (assert-true
  (= 5/3
     (let ((*read-base* 16))
       (read-from-string "23/15")))))
(define-test sacla-must-reader.633 (:tag :sacla)
 (assert-true
  (= 35/22
     (let ((*read-base* 16))
       (read-from-string "23/16")))))
(define-test sacla-must-reader.634 (:tag :sacla)
 (assert-true
  (= 35/23
     (let ((*read-base* 16))
       (read-from-string "23/17")))))
(define-test sacla-must-reader.635 (:tag :sacla)
 (assert-true
  (= 35/24
     (let ((*read-base* 16))
       (read-from-string "23/18")))))
(define-test sacla-must-reader.636 (:tag :sacla)
 (assert-true
  (= 7/5
     (let ((*read-base* 16))
       (read-from-string "23/19")))))
(define-test sacla-must-reader.637 (:tag :sacla)
 (assert-true
  (= 35/26
     (let ((*read-base* 16))
       (read-from-string "23/1A")))))
(define-test sacla-must-reader.638 (:tag :sacla)
 (assert-true
  (= 35/27
     (let ((*read-base* 16))
       (read-from-string "23/1B")))))
(define-test sacla-must-reader.639 (:tag :sacla)
 (assert-true
  (= 5/4
     (let ((*read-base* 16))
       (read-from-string "23/1C")))))
(define-test sacla-must-reader.640 (:tag :sacla)
 (assert-true
  (= 35/29
     (let ((*read-base* 16))
       (read-from-string "23/1D")))))
(define-test sacla-must-reader.641 (:tag :sacla)
 (assert-true
  (= 7/6
     (let ((*read-base* 16))
       (read-from-string "23/1E")))))
(define-test sacla-must-reader.642 (:tag :sacla)
 (assert-true
  (= 35/31
     (let ((*read-base* 16))
       (read-from-string "23/1F")))))
(define-test sacla-must-reader.643 (:tag :sacla)
 (assert-true
  (= 35/32
     (let ((*read-base* 16))
       (read-from-string "23/20")))))
(define-test sacla-must-reader.644 (:tag :sacla)
 (assert-true
  (= 35/33
     (let ((*read-base* 16))
       (read-from-string "23/21")))))
(define-test sacla-must-reader.645 (:tag :sacla)
 (assert-true
  (= 35/34
     (let ((*read-base* 16))
       (read-from-string "23/22")))))
(define-test sacla-must-reader.646 (:tag :sacla)
 (assert-true
  (= 1
     (let ((*read-base* 16))
       (read-from-string "23/23")))))
(define-test sacla-must-reader.647 (:tag :sacla)
 (assert-true
  (= 35/36
     (let ((*read-base* 16))
       (read-from-string "23/24")))))
(define-test sacla-must-reader.648 (:tag :sacla)
 (assert-true
  (= 55
     (let ((*read-base* 16))
       (read-from-string "6E/2")))))
(define-test sacla-must-reader.649 (:tag :sacla)
 (assert-true
  (= 110/3
     (let ((*read-base* 16))
       (read-from-string "6E/3")))))
(define-test sacla-must-reader.650 (:tag :sacla)
 (assert-true
  (= 55/2
     (let ((*read-base* 16))
       (read-from-string "6E/4")))))
(define-test sacla-must-reader.651 (:tag :sacla)
 (assert-true
  (= 22
     (let ((*read-base* 16))
       (read-from-string "6E/5")))))
(define-test sacla-must-reader.652 (:tag :sacla)
 (assert-true
  (= 55/3
     (let ((*read-base* 16))
       (read-from-string "6E/6")))))
(define-test sacla-must-reader.653 (:tag :sacla)
 (assert-true
  (= 110/7
     (let ((*read-base* 16))
       (read-from-string "6E/7")))))
(define-test sacla-must-reader.654 (:tag :sacla)
 (assert-true
  (= 55/4
     (let ((*read-base* 16))
       (read-from-string "6E/8")))))
(define-test sacla-must-reader.655 (:tag :sacla)
 (assert-true
  (= 110/9
     (let ((*read-base* 16))
       (read-from-string "6E/9")))))
(define-test sacla-must-reader.656 (:tag :sacla)
 (assert-true
  (= 11
     (let ((*read-base* 16))
       (read-from-string "6E/A")))))
(define-test sacla-must-reader.657 (:tag :sacla)
 (assert-true
  (= 10
     (let ((*read-base* 16))
       (read-from-string "6E/B")))))
(define-test sacla-must-reader.658 (:tag :sacla)
 (assert-true
  (= 55/6
     (let ((*read-base* 16))
       (read-from-string "6E/C")))))
(define-test sacla-must-reader.659 (:tag :sacla)
 (assert-true
  (= 110/13
     (let ((*read-base* 16))
       (read-from-string "6E/D")))))
(define-test sacla-must-reader.660 (:tag :sacla)
 (assert-true
  (= 55/7
     (let ((*read-base* 16))
       (read-from-string "6E/E")))))
(define-test sacla-must-reader.661 (:tag :sacla)
 (assert-true
  (= 22/3
     (let ((*read-base* 16))
       (read-from-string "6E/F")))))
(define-test sacla-must-reader.662 (:tag :sacla)
 (assert-true
  (= 11
     (let ((*read-base* 16))
       (read-from-string "6E/a")))))
(define-test sacla-must-reader.663 (:tag :sacla)
 (assert-true
  (= 10
     (let ((*read-base* 16))
       (read-from-string "6E/b")))))
(define-test sacla-must-reader.664 (:tag :sacla)
 (assert-true
  (= 55/6
     (let ((*read-base* 16))
       (read-from-string "6E/c")))))
(define-test sacla-must-reader.665 (:tag :sacla)
 (assert-true
  (= 110/13
     (let ((*read-base* 16))
       (read-from-string "6E/d")))))
(define-test sacla-must-reader.666 (:tag :sacla)
 (assert-true
  (= 55/7
     (let ((*read-base* 16))
       (read-from-string "6E/e")))))
(define-test sacla-must-reader.667 (:tag :sacla)
 (assert-true
  (= 22/3
     (let ((*read-base* 16))
       (read-from-string "6E/f")))))
(define-test sacla-must-reader.668 (:tag :sacla)
 (assert-true
  (= 55/8
     (let ((*read-base* 16))
       (read-from-string "6E/10")))))
(define-test sacla-must-reader.669 (:tag :sacla)
 (assert-true
  (= 110/17
     (let ((*read-base* 16))
       (read-from-string "6E/11")))))
(define-test sacla-must-reader.670 (:tag :sacla)
 (assert-true
  (= 55/9
     (let ((*read-base* 16))
       (read-from-string "6E/12")))))
(define-test sacla-must-reader.671 (:tag :sacla)
 (assert-true
  (= 110/19
     (let ((*read-base* 16))
       (read-from-string "6E/13")))))
(define-test sacla-must-reader.672 (:tag :sacla)
 (assert-true
  (= 11/2
     (let ((*read-base* 16))
       (read-from-string "6E/14")))))
(define-test sacla-must-reader.673 (:tag :sacla)
 (assert-true
  (= 110/21
     (let ((*read-base* 16))
       (read-from-string "6E/15")))))
(define-test sacla-must-reader.674 (:tag :sacla)
 (assert-true
  (= 5
     (let ((*read-base* 16))
       (read-from-string "6E/16")))))
(define-test sacla-must-reader.675 (:tag :sacla)
 (assert-true
  (= 110/23
     (let ((*read-base* 16))
       (read-from-string "6E/17")))))
(define-test sacla-must-reader.676 (:tag :sacla)
 (assert-true
  (= 55/12
     (let ((*read-base* 16))
       (read-from-string "6E/18")))))
(define-test sacla-must-reader.677 (:tag :sacla)
 (assert-true
  (= 22/5
     (let ((*read-base* 16))
       (read-from-string "6E/19")))))
(define-test sacla-must-reader.678 (:tag :sacla)
 (assert-true
  (= 55/13
     (let ((*read-base* 16))
       (read-from-string "6E/1A")))))
(define-test sacla-must-reader.679 (:tag :sacla)
 (assert-true
  (= 110/27
     (let ((*read-base* 16))
       (read-from-string "6E/1B")))))
(define-test sacla-must-reader.680 (:tag :sacla)
 (assert-true
  (= 55/14
     (let ((*read-base* 16))
       (read-from-string "6E/1C")))))
(define-test sacla-must-reader.681 (:tag :sacla)
 (assert-true
  (= 110/29
     (let ((*read-base* 16))
       (read-from-string "6E/1D")))))
(define-test sacla-must-reader.682 (:tag :sacla)
 (assert-true
  (= 11/3
     (let ((*read-base* 16))
       (read-from-string "6E/1E")))))
(define-test sacla-must-reader.683 (:tag :sacla)
 (assert-true
  (= 110/31
     (let ((*read-base* 16))
       (read-from-string "6E/1F")))))
(define-test sacla-must-reader.684 (:tag :sacla)
 (assert-true
  (= 55/16
     (let ((*read-base* 16))
       (read-from-string "6E/20")))))
(define-test sacla-must-reader.685 (:tag :sacla)
 (assert-true
  (= 10/3
     (let ((*read-base* 16))
       (read-from-string "6E/21")))))
(define-test sacla-must-reader.686 (:tag :sacla)
 (assert-true
  (= 55/17
     (let ((*read-base* 16))
       (read-from-string "6E/22")))))
(define-test sacla-must-reader.687 (:tag :sacla)
 (assert-true
  (= 22/7
     (let ((*read-base* 16))
       (read-from-string "6E/23")))))
(define-test sacla-must-reader.688 (:tag :sacla)
 (assert-true
  (= 55/18
     (let ((*read-base* 16))
       (read-from-string "6E/24")))))
(define-test sacla-must-reader.689 (:tag :sacla)
 (assert-true
  (= 1/101010101010101010101010101010101
     (read-from-string "11/1111111111111111111111111111111111"))))
(define-test sacla-must-reader.690 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "0.0")))
    (and (floatp f) (zerop f)))))
(define-test sacla-must-reader.691 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+0.0")))
    (and (floatp f) (zerop f)))))
(define-test sacla-must-reader.692 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-0.0")))
    (and (floatp f) (zerop f)))))
(define-test sacla-must-reader.693 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string ".0")))
    (and (floatp f) (zerop f)))))
(define-test sacla-must-reader.694 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+.0")))
    (and (floatp f) (zerop f)))))
(define-test sacla-must-reader.695 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-.0")))
    (and (floatp f) (zerop f)))))
(define-test sacla-must-reader.696 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1.0")))
    (and (floatp f) (= 1.0 f)))))
(define-test sacla-must-reader.697 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1.0")))
    (and (floatp f) (= 1.0 f)))))
(define-test sacla-must-reader.698 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1.0")))
    (and (floatp f) (= -1.0 f)))))
(define-test sacla-must-reader.699 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1d1")))
    (and (floatp f) (= 10.0d0 f)))))
(define-test sacla-must-reader.700 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1e1")))
    (and (floatp f) (= 10.0 f)))))
(define-test sacla-must-reader.701 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1f1")))
    (and (floatp f) (= 10.0 f)))))
(define-test sacla-must-reader.702 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1l1")))
    (and (floatp f) (= 10.0d0 f)))))
(define-test sacla-must-reader.703 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1s1")))
    (and (floatp f) (= 10.0 f)))))
(define-test sacla-must-reader.704 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1D1")))
    (and (floatp f) (= 10.0d0 f)))))
(define-test sacla-must-reader.705 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1E1")))
    (and (floatp f) (= 10.0 f)))))
(define-test sacla-must-reader.706 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1F1")))
    (and (floatp f) (= 10.0 f)))))
(define-test sacla-must-reader.707 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1L1")))
    (and (floatp f) (= 10.0d0 f)))))
(define-test sacla-must-reader.708 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1S1")))
    (and (floatp f) (= 10.0 f)))))
(define-test sacla-must-reader.709 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1d+1")))
    (and (floatp f) (= 10.0d0 f)))))
(define-test sacla-must-reader.710 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1e+1")))
    (and (floatp f) (= 10.0 f)))))
(define-test sacla-must-reader.711 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1f+1")))
    (and (floatp f) (= 10.0 f)))))
(define-test sacla-must-reader.712 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1l+1")))
    (and (floatp f) (= 10.0d0 f)))))
(define-test sacla-must-reader.713 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1s+1")))
    (and (floatp f) (= 10.0 f)))))
(define-test sacla-must-reader.714 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1D+1")))
    (and (floatp f) (= 10.0d0 f)))))
(define-test sacla-must-reader.715 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1E+1")))
    (and (floatp f) (= 10.0 f)))))
(define-test sacla-must-reader.716 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1F+1")))
    (and (floatp f) (= 10.0 f)))))
(define-test sacla-must-reader.717 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1L+1")))
    (and (floatp f) (= 10.0d0 f)))))
(define-test sacla-must-reader.718 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1S+1")))
    (and (floatp f) (= 10.0 f)))))
(define-test sacla-must-reader.719 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1d-1")))
    (and (floatp f) (= 0.1d0 f)))))
(define-test sacla-must-reader.720 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1e-1")))
    (and (floatp f) (= 0.1 f)))))
(define-test sacla-must-reader.721 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1f-1")))
    (and (floatp f) (= 0.1 f)))))
(define-test sacla-must-reader.722 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1l-1")))
    (and (floatp f) (= 0.1d0 f)))))
(define-test sacla-must-reader.723 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1s-1")))
    (and (floatp f) (= 0.1 f)))))
(define-test sacla-must-reader.724 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1D-1")))
    (and (floatp f) (= 0.1d0 f)))))
(define-test sacla-must-reader.725 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1E-1")))
    (and (floatp f) (= 0.1 f)))))
(define-test sacla-must-reader.726 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1F-1")))
    (and (floatp f) (= 0.1 f)))))
(define-test sacla-must-reader.727 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1L-1")))
    (and (floatp f) (= 0.1d0 f)))))
(define-test sacla-must-reader.728 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1S-1")))
    (and (floatp f) (= 0.1 f)))))
(define-test sacla-must-reader.729 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1d1")))
    (and (floatp f) (= 10.0d0 f)))))
(define-test sacla-must-reader.730 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1e1")))
    (and (floatp f) (= 10.0 f)))))
(define-test sacla-must-reader.731 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1f1")))
    (and (floatp f) (= 10.0 f)))))
(define-test sacla-must-reader.732 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1l1")))
    (and (floatp f) (= 10.0d0 f)))))
(define-test sacla-must-reader.733 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1s1")))
    (and (floatp f) (= 10.0 f)))))
(define-test sacla-must-reader.734 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1D1")))
    (and (floatp f) (= 10.0d0 f)))))
(define-test sacla-must-reader.735 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1E1")))
    (and (floatp f) (= 10.0 f)))))
(define-test sacla-must-reader.736 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1F1")))
    (and (floatp f) (= 10.0 f)))))
(define-test sacla-must-reader.737 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1L1")))
    (and (floatp f) (= 10.0d0 f)))))
(define-test sacla-must-reader.738 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1S1")))
    (and (floatp f) (= 10.0 f)))))
(define-test sacla-must-reader.739 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1d+1")))
    (and (floatp f) (= 10.0d0 f)))))
(define-test sacla-must-reader.740 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1e+1")))
    (and (floatp f) (= 10.0 f)))))
(define-test sacla-must-reader.741 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1f+1")))
    (and (floatp f) (= 10.0 f)))))
(define-test sacla-must-reader.742 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1l+1")))
    (and (floatp f) (= 10.0d0 f)))))
(define-test sacla-must-reader.743 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1s+1")))
    (and (floatp f) (= 10.0 f)))))
(define-test sacla-must-reader.744 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1D+1")))
    (and (floatp f) (= 10.0d0 f)))))
(define-test sacla-must-reader.745 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1E+1")))
    (and (floatp f) (= 10.0 f)))))
(define-test sacla-must-reader.746 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1F+1")))
    (and (floatp f) (= 10.0 f)))))
(define-test sacla-must-reader.747 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1L+1")))
    (and (floatp f) (= 10.0d0 f)))))
(define-test sacla-must-reader.748 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1S+1")))
    (and (floatp f) (= 10.0 f)))))
(define-test sacla-must-reader.749 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1d-1")))
    (and (floatp f) (= 0.1d0 f)))))
(define-test sacla-must-reader.750 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1e-1")))
    (and (floatp f) (= 0.1 f)))))
(define-test sacla-must-reader.751 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1f-1")))
    (and (floatp f) (= 0.1 f)))))
(define-test sacla-must-reader.752 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1l-1")))
    (and (floatp f) (= 0.1d0 f)))))
(define-test sacla-must-reader.753 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1s-1")))
    (and (floatp f) (= 0.1 f)))))
(define-test sacla-must-reader.754 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1D-1")))
    (and (floatp f) (= 0.1d0 f)))))
(define-test sacla-must-reader.755 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1E-1")))
    (and (floatp f) (= 0.1 f)))))
(define-test sacla-must-reader.756 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1F-1")))
    (and (floatp f) (= 0.1 f)))))
(define-test sacla-must-reader.757 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1L-1")))
    (and (floatp f) (= 0.1d0 f)))))
(define-test sacla-must-reader.758 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1S-1")))
    (and (floatp f) (= 0.1 f)))))
(define-test sacla-must-reader.759 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1d1")))
    (and (floatp f) (= -10.0d0 f)))))
(define-test sacla-must-reader.760 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1e1")))
    (and (floatp f) (= -10.0 f)))))
(define-test sacla-must-reader.761 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1f1")))
    (and (floatp f) (= -10.0 f)))))
(define-test sacla-must-reader.762 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1l1")))
    (and (floatp f) (= -10.0d0 f)))))
(define-test sacla-must-reader.763 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1s1")))
    (and (floatp f) (= -10.0 f)))))
(define-test sacla-must-reader.764 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1D1")))
    (and (floatp f) (= -10.0d0 f)))))
(define-test sacla-must-reader.765 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1E1")))
    (and (floatp f) (= -10.0 f)))))
(define-test sacla-must-reader.766 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1F1")))
    (and (floatp f) (= -10.0 f)))))
(define-test sacla-must-reader.767 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1L1")))
    (and (floatp f) (= -10.0d0 f)))))
(define-test sacla-must-reader.768 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1S1")))
    (and (floatp f) (= -10.0 f)))))
(define-test sacla-must-reader.769 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1d+1")))
    (and (floatp f) (= -10.0d0 f)))))
(define-test sacla-must-reader.770 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1e+1")))
    (and (floatp f) (= -10.0 f)))))
(define-test sacla-must-reader.771 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1f+1")))
    (and (floatp f) (= -10.0 f)))))
(define-test sacla-must-reader.772 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1l+1")))
    (and (floatp f) (= -10.0d0 f)))))
(define-test sacla-must-reader.773 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1s+1")))
    (and (floatp f) (= -10.0 f)))))
(define-test sacla-must-reader.774 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1D+1")))
    (and (floatp f) (= -10.0d0 f)))))
(define-test sacla-must-reader.775 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1E+1")))
    (and (floatp f) (= -10.0 f)))))
(define-test sacla-must-reader.776 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1F+1")))
    (and (floatp f) (= -10.0 f)))))
(define-test sacla-must-reader.777 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1L+1")))
    (and (floatp f) (= -10.0d0 f)))))
(define-test sacla-must-reader.778 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1S+1")))
    (and (floatp f) (= -10.0 f)))))
(define-test sacla-must-reader.779 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1d-1")))
    (and (floatp f) (= -0.1d0 f)))))
(define-test sacla-must-reader.780 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1e-1")))
    (and (floatp f) (= -0.1 f)))))
(define-test sacla-must-reader.781 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1f-1")))
    (and (floatp f) (= -0.1 f)))))
(define-test sacla-must-reader.782 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1l-1")))
    (and (floatp f) (= -0.1d0 f)))))
(define-test sacla-must-reader.783 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1s-1")))
    (and (floatp f) (= -0.1 f)))))
(define-test sacla-must-reader.784 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1D-1")))
    (and (floatp f) (= -0.1d0 f)))))
(define-test sacla-must-reader.785 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1E-1")))
    (and (floatp f) (= -0.1 f)))))
(define-test sacla-must-reader.786 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1F-1")))
    (and (floatp f) (= -0.1 f)))))
(define-test sacla-must-reader.787 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1L-1")))
    (and (floatp f) (= -0.1d0 f)))))
(define-test sacla-must-reader.788 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1S-1")))
    (and (floatp f) (= -0.1 f)))))
(define-test sacla-must-reader.789 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1d10")))
    (and (floatp f) (= 1.0d10 f)))))
(define-test sacla-must-reader.790 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1e10")))
    (and (floatp f) (= 1.0e10 f)))))
(define-test sacla-must-reader.791 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1f10")))
    (and (floatp f) (= 1.0e10 f)))))
(define-test sacla-must-reader.792 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1l10")))
    (and (floatp f) (= 1.0d10 f)))))
(define-test sacla-must-reader.793 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1s10")))
    (and (floatp f) (= 1.0e10 f)))))
(define-test sacla-must-reader.794 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1D10")))
    (and (floatp f) (= 1.0d10 f)))))
(define-test sacla-must-reader.795 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1E10")))
    (and (floatp f) (= 1.0e10 f)))))
(define-test sacla-must-reader.796 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1F10")))
    (and (floatp f) (= 1.0e10 f)))))
(define-test sacla-must-reader.797 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1L10")))
    (and (floatp f) (= 1.0d10 f)))))
(define-test sacla-must-reader.798 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1S10")))
    (and (floatp f) (= 1.0e10 f)))))
(define-test sacla-must-reader.799 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1d+10")))
    (and (floatp f) (= 1.0d10 f)))))
(define-test sacla-must-reader.800 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1e+10")))
    (and (floatp f) (= 1.0e10 f)))))
(define-test sacla-must-reader.801 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1f+10")))
    (and (floatp f) (= 1.0e10 f)))))
(define-test sacla-must-reader.802 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1l+10")))
    (and (floatp f) (= 1.0d10 f)))))
(define-test sacla-must-reader.803 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1s+10")))
    (and (floatp f) (= 1.0e10 f)))))
(define-test sacla-must-reader.804 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1D+10")))
    (and (floatp f) (= 1.0d10 f)))))
(define-test sacla-must-reader.805 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1E+10")))
    (and (floatp f) (= 1.0e10 f)))))
(define-test sacla-must-reader.806 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1F+10")))
    (and (floatp f) (= 1.0e10 f)))))
(define-test sacla-must-reader.807 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1L+10")))
    (and (floatp f) (= 1.0d10 f)))))
(define-test sacla-must-reader.808 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1S+10")))
    (and (floatp f) (= 1.0e10 f)))))
(define-test sacla-must-reader.809 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1d-10")))
    (and (floatp f) (= 1.0d-10 f)))))
(define-test sacla-must-reader.810 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1e-10")))
    (and (floatp f) (= 1.0e-10 f)))))
(define-test sacla-must-reader.811 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1f-10")))
    (and (floatp f) (= 1.0e-10 f)))))
(define-test sacla-must-reader.812 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1l-10")))
    (and (floatp f) (= 1.0d-10 f)))))
(define-test sacla-must-reader.813 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1s-10")))
    (and (floatp f) (= 1.0e-10 f)))))
(define-test sacla-must-reader.814 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1D-10")))
    (and (floatp f) (= 1.0d-10 f)))))
(define-test sacla-must-reader.815 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1E-10")))
    (and (floatp f) (= 1.0e-10 f)))))
(define-test sacla-must-reader.816 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1F-10")))
    (and (floatp f) (= 1.0e-10 f)))))
(define-test sacla-must-reader.817 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1L-10")))
    (and (floatp f) (= 1.0d-10 f)))))
(define-test sacla-must-reader.818 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "1S-10")))
    (and (floatp f) (= 1.0e-10 f)))))
(define-test sacla-must-reader.819 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1d10")))
    (and (floatp f) (= 1.0d10 f)))))
(define-test sacla-must-reader.820 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1e10")))
    (and (floatp f) (= 1.0e10 f)))))
(define-test sacla-must-reader.821 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1f10")))
    (and (floatp f) (= 1.0e10 f)))))
(define-test sacla-must-reader.822 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1l10")))
    (and (floatp f) (= 1.0d10 f)))))
(define-test sacla-must-reader.823 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1s10")))
    (and (floatp f) (= 1.0e10 f)))))
(define-test sacla-must-reader.824 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1D10")))
    (and (floatp f) (= 1.0d10 f)))))
(define-test sacla-must-reader.825 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1E10")))
    (and (floatp f) (= 1.0e10 f)))))
(define-test sacla-must-reader.826 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1F10")))
    (and (floatp f) (= 1.0e10 f)))))
(define-test sacla-must-reader.827 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1L10")))
    (and (floatp f) (= 1.0d10 f)))))
(define-test sacla-must-reader.828 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1S10")))
    (and (floatp f) (= 1.0e10 f)))))
(define-test sacla-must-reader.829 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1d+10")))
    (and (floatp f) (= 1.0d10 f)))))
(define-test sacla-must-reader.830 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1e+10")))
    (and (floatp f) (= 1.0e10 f)))))
(define-test sacla-must-reader.831 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1f+10")))
    (and (floatp f) (= 1.0e10 f)))))
(define-test sacla-must-reader.832 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1l+10")))
    (and (floatp f) (= 1.0d10 f)))))
(define-test sacla-must-reader.833 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1s+10")))
    (and (floatp f) (= 1.0e10 f)))))
(define-test sacla-must-reader.834 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1D+10")))
    (and (floatp f) (= 1.0d10 f)))))
(define-test sacla-must-reader.835 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1E+10")))
    (and (floatp f) (= 1.0e10 f)))))
(define-test sacla-must-reader.836 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1F+10")))
    (and (floatp f) (= 1.0e10 f)))))
(define-test sacla-must-reader.837 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1L+10")))
    (and (floatp f) (= 1.0d10 f)))))
(define-test sacla-must-reader.838 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1S+10")))
    (and (floatp f) (= 1.0e10 f)))))
(define-test sacla-must-reader.839 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1d-10")))
    (and (floatp f) (= 1.0d-10 f)))))
(define-test sacla-must-reader.840 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1e-10")))
    (and (floatp f) (= 1.0e-10 f)))))
(define-test sacla-must-reader.841 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1f-10")))
    (and (floatp f) (= 1.0e-10 f)))))
(define-test sacla-must-reader.842 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1l-10")))
    (and (floatp f) (= 1.0d-10 f)))))
(define-test sacla-must-reader.843 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1s-10")))
    (and (floatp f) (= 1.0e-10 f)))))
(define-test sacla-must-reader.844 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1D-10")))
    (and (floatp f) (= 1.0d-10 f)))))
(define-test sacla-must-reader.845 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1E-10")))
    (and (floatp f) (= 1.0e-10 f)))))
(define-test sacla-must-reader.846 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1F-10")))
    (and (floatp f) (= 1.0e-10 f)))))
(define-test sacla-must-reader.847 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1L-10")))
    (and (floatp f) (= 1.0d-10 f)))))
(define-test sacla-must-reader.848 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "+1S-10")))
    (and (floatp f) (= 1.0e-10 f)))))
(define-test sacla-must-reader.849 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1d10")))
    (and (floatp f) (= -1.0d10 f)))))
(define-test sacla-must-reader.850 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1e10")))
    (and (floatp f) (= -1.0e10 f)))))
(define-test sacla-must-reader.851 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1f10")))
    (and (floatp f) (= -1.0e10 f)))))
(define-test sacla-must-reader.852 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1l10")))
    (and (floatp f) (= -1.0d10 f)))))
(define-test sacla-must-reader.853 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1s10")))
    (and (floatp f) (= -1.0e10 f)))))
(define-test sacla-must-reader.854 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1D10")))
    (and (floatp f) (= -1.0d10 f)))))
(define-test sacla-must-reader.855 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1E10")))
    (and (floatp f) (= -1.0e10 f)))))
(define-test sacla-must-reader.856 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1F10")))
    (and (floatp f) (= -1.0e10 f)))))
(define-test sacla-must-reader.857 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1L10")))
    (and (floatp f) (= -1.0d10 f)))))
(define-test sacla-must-reader.858 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1S10")))
    (and (floatp f) (= -1.0e10 f)))))
(define-test sacla-must-reader.859 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1d+10")))
    (and (floatp f) (= -1.0d10 f)))))
(define-test sacla-must-reader.860 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1e+10")))
    (and (floatp f) (= -1.0e10 f)))))
(define-test sacla-must-reader.861 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1f+10")))
    (and (floatp f) (= -1.0e10 f)))))
(define-test sacla-must-reader.862 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1l+10")))
    (and (floatp f) (= -1.0d10 f)))))
(define-test sacla-must-reader.863 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1s+10")))
    (and (floatp f) (= -1.0e10 f)))))
(define-test sacla-must-reader.864 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1D+10")))
    (and (floatp f) (= -1.0d10 f)))))
(define-test sacla-must-reader.865 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1E+10")))
    (and (floatp f) (= -1.0e10 f)))))
(define-test sacla-must-reader.866 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1F+10")))
    (and (floatp f) (= -1.0e10 f)))))
(define-test sacla-must-reader.867 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1L+10")))
    (and (floatp f) (= -1.0d10 f)))))
(define-test sacla-must-reader.868 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1S+10")))
    (and (floatp f) (= -1.0e10 f)))))
(define-test sacla-must-reader.869 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1d-10")))
    (and (floatp f) (= -1.0d-10 f)))))
(define-test sacla-must-reader.870 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1e-10")))
    (and (floatp f) (= -1.0e-10 f)))))
(define-test sacla-must-reader.871 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1f-10")))
    (and (floatp f) (= -1.0e-10 f)))))
(define-test sacla-must-reader.872 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1l-10")))
    (and (floatp f) (= -1.0d-10 f)))))
(define-test sacla-must-reader.873 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1s-10")))
    (and (floatp f) (= -1.0e-10 f)))))
(define-test sacla-must-reader.874 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1D-10")))
    (and (floatp f) (= -1.0d-10 f)))))
(define-test sacla-must-reader.875 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1E-10")))
    (and (floatp f) (= -1.0e-10 f)))))
(define-test sacla-must-reader.876 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1F-10")))
    (and (floatp f) (= -1.0e-10 f)))))
(define-test sacla-must-reader.877 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1L-10")))
    (and (floatp f) (= -1.0d-10 f)))))
(define-test sacla-must-reader.878 (:tag :sacla)
 (assert-true
  (let ((f (read-from-string "-1S-10")))
    (and (floatp f) (= -1.0e-10 f)))))
(define-test sacla-must-reader.879 (:tag :sacla)
 (assert-true (floatp (read-from-string "-1.23"))))
(define-test sacla-must-reader.880 (:tag :sacla)
 (assert-true (floatp (read-from-string "-823.0023D10"))))
(define-test sacla-must-reader.881 (:tag :sacla)
 (assert-true (floatp (read-from-string "-324.0293E10"))))
(define-test sacla-must-reader.882 (:tag :sacla)
 (assert-true (floatp (read-from-string "-12.0023F10"))))
(define-test sacla-must-reader.883 (:tag :sacla)
 (assert-true (floatp (read-from-string "-911.823L10"))))
(define-test sacla-must-reader.884 (:tag :sacla)
 (assert-true (floatp (read-from-string "-788.823S10"))))
(define-test sacla-must-reader.885 (:tag :sacla)
 (assert-true (eq '|256| (read-from-string "\\256"))))
(define-test sacla-must-reader.886 (:tag :sacla)
 (assert-true (eq '|2564| (read-from-string "25\\64"))))
(define-test sacla-must-reader.887 (:tag :sacla)
 (assert-true (eq '|1.0E6| (read-from-string "1.0\\E6"))))
(define-test sacla-must-reader.888 (:tag :sacla)
 (assert-true (eq '|100| (read-from-string "|100|"))))
(define-test sacla-must-reader.889 (:tag :sacla)
 (assert-true (eq '|3.14159| (read-from-string "3\\.14159"))))
(define-test sacla-must-reader.890 (:tag :sacla)
 (assert-true (eq '|3/4| (read-from-string "|3/4|"))))
(define-test sacla-must-reader.891 (:tag :sacla)
 (assert-true (eq '|3/4| (read-from-string "3\\/4"))))
(define-test sacla-must-reader.892 (:tag :sacla)
 (assert-true (eq '|5| (read-from-string "5||"))))
(define-test sacla-must-reader.893 (:tag :sacla)
 (assert-true (eq '|5| (read-from-string "||5"))))
(define-test sacla-must-reader.894 (:tag :sacla)
 (assert-true (eq '|567| (read-from-string "||567"))))
(define-test sacla-must-reader.895 (:tag :sacla)
 (assert-true (eq '|567| (read-from-string "5||67"))))
(define-test sacla-must-reader.896 (:tag :sacla)
 (assert-true (eq '|567| (read-from-string "56||7"))))
(define-test sacla-must-reader.897 (:tag :sacla)
 (assert-true (eq '|567| (read-from-string "567||"))))
(define-test sacla-must-reader.898 (:tag :sacla)
 (assert-true (eq '|567| (read-from-string "||5||6||7||"))))
(define-test sacla-must-reader.899 (:tag :sacla)
 (assert-true (eq '|567| (read-from-string "||||5||||6||||7||||"))))
(define-test sacla-must-reader.900 (:tag :sacla)
 (assert-true (eq '|567| (read-from-string "567||||||"))))
(define-test sacla-must-reader.901 (:tag :sacla)
 (assert-true (eq '/ (read-from-string "/"))))
(define-test sacla-must-reader.902 (:tag :sacla)
 (assert-true (eq '/5 (read-from-string "/5"))))
(define-test sacla-must-reader.903 (:tag :sacla)
 (assert-true (eq '+ (read-from-string "+"))))
(define-test sacla-must-reader.904 (:tag :sacla)
 (assert-true (eq '1+ (read-from-string "1+"))))
(define-test sacla-must-reader.905 (:tag :sacla)
 (assert-true (eq '1- (read-from-string "1-"))))
(define-test sacla-must-reader.906 (:tag :sacla)
 (assert-true (eq 'foo+ (read-from-string "foo+"))))
(define-test sacla-must-reader.907 (:tag :sacla)
 (assert-true (eq 'ab.cd (read-from-string "ab.cd"))))
(define-test sacla-must-reader.908 (:tag :sacla)
 (assert-true (eq '_ (read-from-string "_"))))
(define-test sacla-must-reader.909 (:tag :sacla)
 (assert-true (eq '^ (read-from-string "^"))))
(define-test sacla-must-reader.910 (:tag :sacla)
 (assert-true (eq '^/- (read-from-string "^/-"))))
(define-test sacla-must-reader.911 (:tag :sacla)
 (assert-true (eq :a (read-from-string ":a"))))
(define-test sacla-must-reader.912 (:tag :sacla)
 (assert-true (eq :b (read-from-string ":b"))))
(define-test sacla-must-reader.913 (:tag :sacla)
 (assert-true (eq :c (read-from-string ":c"))))
(define-test sacla-must-reader.914 (:tag :sacla)
 (assert-true (eq :d (read-from-string ":d"))))
(define-test sacla-must-reader.915 (:tag :sacla)
 (assert-true (eq :keyword-symbol (read-from-string ":keyword-symbol"))))
(define-test sacla-must-reader.916 (:tag :sacla)
 (assert-true (eq 'cdr (read-from-string "cl::cdr"))))
(define-test sacla-must-reader.917 (:tag :sacla)
 (assert-true (eq 'append (read-from-string "cl:append"))))
(define-test sacla-must-reader.918 (:tag :sacla)
 (assert-true (eq 'append (read-from-string "cl-user::append"))))
(define-test sacla-must-reader.919 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package 'test-foo)
      (delete-package 'test-foo))
    (make-package 'test-foo :use nil)
    (handler-case (read-from-string "test-foo:no-such-symbol")
      (error nil t)
      (:no-error (&rest rest) (declare (ignore rest)) nil)))))
(define-test sacla-must-reader.920 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package 'test-foo)
      (delete-package 'test-foo))
    (make-package 'test-foo :use nil)
    (and (not (find-symbol "NEW-ONE" "TEST-FOO"))
         (read-from-string "test-foo::new-one")
         (find-symbol "NEW-ONE" "TEST-FOO")))))
(define-test sacla-must-reader.921 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package 'test-foo)
      (delete-package 'test-foo))
    (let ((*package* (make-package 'test-foo :use nil)))
      (read-from-string "my-symbol")))))
(define-test sacla-must-reader.922 (:tag :sacla)
 (assert-true (string= " " (symbol-name (read-from-string "cl-user::\\ ")))))
(define-test sacla-must-reader.923 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package 'no-such-package)
      (delete-package 'no-such-package))
    (handler-case (read-from-string "no-such-package::bar")
      (error nil t)
      (:no-error (&rest rest) (declare (ignore rest)) nil)))))
(define-test sacla-must-reader.924 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package 'no-such-package)
      (delete-package 'no-such-package))
    (handler-case (read-from-string "no-such-package::no-such-symbol")
      (error nil t)
      (:no-error (&rest rest) (declare (ignore rest)) nil)))))
(define-test sacla-must-reader.925 (:tag :sacla)
 (assert-true (string= "FROBBOZ" (symbol-name (read-from-string "FROBBOZ")))))
(define-test sacla-must-reader.926 (:tag :sacla)
 (assert-true (string= "FROBBOZ" (symbol-name (read-from-string "frobboz")))))
(define-test sacla-must-reader.927 (:tag :sacla)
 (assert-true (string= "FROBBOZ" (symbol-name (read-from-string "fRObBoz")))))
(define-test sacla-must-reader.928 (:tag :sacla)
 (assert-true
  (string= "UNWIND-PROTECT" (symbol-name (read-from-string "unwind-protect")))))
(define-test sacla-must-reader.929 (:tag :sacla)
 (assert-true (string= "+$" (symbol-name (read-from-string "+$")))))
(define-test sacla-must-reader.930 (:tag :sacla)
 (assert-true (string= "1+" (symbol-name (read-from-string "1+")))))
(define-test sacla-must-reader.931 (:tag :sacla)
 (assert-true (= 1 (read-from-string "+1"))))
(define-test sacla-must-reader.932 (:tag :sacla)
 (assert-true
  (string= "PASCAL_STYLE" (symbol-name (read-from-string "pascal_style")))))
(define-test sacla-must-reader.933 (:tag :sacla)
 (assert-true
  (string= "FILE.REL.43" (symbol-name (read-from-string "file.rel.43")))))
(define-test sacla-must-reader.934 (:tag :sacla)
 (assert-true (string= "(" (symbol-name (read-from-string "\\(")))))
(define-test sacla-must-reader.935 (:tag :sacla)
 (assert-true (string= "+1" (symbol-name (read-from-string "\\+1")))))
(define-test sacla-must-reader.936 (:tag :sacla)
 (assert-true (string= "+1" (symbol-name (read-from-string "+\\1")))))
(define-test sacla-must-reader.937 (:tag :sacla)
 (assert-true (string= "fROBBOZ" (symbol-name (read-from-string "\\frobboz")))))
(define-test sacla-must-reader.938 (:tag :sacla)
 (assert-true
  (string= "3.14159265s0" (symbol-name (read-from-string "3.14159265\\s0")))))
(define-test sacla-must-reader.939 (:tag :sacla)
 (assert-true
  (string= "3.14159265S0" (symbol-name (read-from-string "3.14159265\\S0")))))
(define-test sacla-must-reader.940 (:tag :sacla)
 (assert-true (string= "FOo" (symbol-name (read-from-string "fo\\o")))))
(define-test sacla-must-reader.941 (:tag :sacla)
 (assert-true
  (string= "APL\\360" (symbol-name (read-from-string "APL\\\\360")))))
(define-test sacla-must-reader.942 (:tag :sacla)
 (assert-true
  (string= "APL\\360" (symbol-name (read-from-string "apl\\\\360")))))
(define-test sacla-must-reader.943 (:tag :sacla)
 (assert-true
  (string= "(B^2)-4*A*C"
           (symbol-name (read-from-string "\\(b^2\\)\\-\\4*a*c")))))
(define-test sacla-must-reader.944 (:tag :sacla)
 (assert-true
  (string= "(b^2)-4*a*c"
           (symbol-name (read-from-string "\\(\\b^2\\)\\-\\4*\\a*\\c")))))
(define-test sacla-must-reader.945 (:tag :sacla)
 (assert-true (string= "\"" (symbol-name (read-from-string "|\"|")))))
(define-test sacla-must-reader.946 (:tag :sacla)
 (assert-true
  (string= "(b^2) - 4*a*c" (symbol-name (read-from-string "|(b^2) - 4*a*c|")))))
(define-test sacla-must-reader.947 (:tag :sacla)
 (assert-true (string= "frobboz" (symbol-name (read-from-string "|frobboz|")))))
(define-test sacla-must-reader.948 (:tag :sacla)
 (assert-true (string= "APL360" (symbol-name (read-from-string "|APL\\360|")))))
(define-test sacla-must-reader.949 (:tag :sacla)
 (assert-true
  (string= "APL\\360" (symbol-name (read-from-string "|APL\\\\360|")))))
(define-test sacla-must-reader.950 (:tag :sacla)
 (assert-true
  (string= "apl\\360" (symbol-name (read-from-string "|apl\\\\360|")))))
(define-test sacla-must-reader.951 (:tag :sacla)
 (assert-true (string= "||" (symbol-name (read-from-string "|\\|\\||")))))
(define-test sacla-must-reader.952 (:tag :sacla)
 (assert-true
  (string= "(B^2) - 4*A*C" (symbol-name (read-from-string "|(B^2) - 4*A*C|")))))
(define-test sacla-must-reader.953 (:tag :sacla)
 (assert-true
  (string= "(b^2) - 4*a*c" (symbol-name (read-from-string "|(b^2) - 4*a*c|")))))
(define-test sacla-must-reader.954 (:tag :sacla)
 (assert-true (string= "." (symbol-name (read-from-string "\\.")))))
(define-test sacla-must-reader.955 (:tag :sacla)
 (assert-true (string= ".." (symbol-name (read-from-string "|..|")))))
(define-test sacla-must-reader.956 (:tag :sacla)
 (assert-true (null (read-from-string "()"))))
(define-test sacla-must-reader.957 (:tag :sacla)
 (assert-true (null (read-from-string "(        )"))))
(define-test sacla-must-reader.958 (:tag :sacla)
 (assert-true (null (read-from-string "(	 	 )"))))
(define-test sacla-must-reader.959 (:tag :sacla)
 (assert-true (equal (read-from-string "(a)") '(a))))
(define-test sacla-must-reader.960 (:tag :sacla)
 (assert-true (equal (read-from-string "( a)") '(a))))
(define-test sacla-must-reader.961 (:tag :sacla)
 (assert-true (equal (read-from-string "(a )") '(a))))
(define-test sacla-must-reader.962 (:tag :sacla)
 (assert-true (equal (read-from-string "(              a           )") '(a))))
(define-test sacla-must-reader.963 (:tag :sacla)
 (assert-true (equal (read-from-string "(a b)") '(a b))))
(define-test sacla-must-reader.964 (:tag :sacla)
 (assert-true (equal (read-from-string "( a b)") '(a b))))
(define-test sacla-must-reader.965 (:tag :sacla)
 (assert-true (equal (read-from-string "( a b )") '(a b))))
(define-test sacla-must-reader.966 (:tag :sacla)
 (assert-true (equal (read-from-string "(  a  b  )") '(a b))))
(define-test sacla-must-reader.967 (:tag :sacla)
 (assert-true (equal (read-from-string "( 	 a 	 b	  )") '(a b))))
(define-test sacla-must-reader.968 (:tag :sacla)
 (assert-true (equal (read-from-string "(a #| |# b)") '(a b))))
(define-test sacla-must-reader.969 (:tag :sacla)
 (assert-true (equal (read-from-string "(a #| |# b #| |# )") '(a b))))
(define-test sacla-must-reader.970 (:tag :sacla)
 (assert-true
  (equal
   (read-from-string "(a #| |# b
)")
   '(a b))))
(define-test sacla-must-reader.971 (:tag :sacla)
 (assert-true
  (equal
   (read-from-string "(
a
b
)")
   '(a b))))
(define-test sacla-must-reader.972 (:tag :sacla)
 (assert-true (equal (read-from-string "(a . b)") '(a . b))))
(define-test sacla-must-reader.973 (:tag :sacla)
 (assert-true (equal (read-from-string "(a . nil)") '(a))))
(define-test sacla-must-reader.974 (:tag :sacla)
 (assert-true (equal (read-from-string "(a . (b))") '(a b))))
(define-test sacla-must-reader.975 (:tag :sacla)
 (assert-true (equal (read-from-string "(a . (b . (c . (d))))") '(a b c d))))
(define-test sacla-must-reader.976 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "(a .$b)")))
    (and (= 2 (length x)) (eq (first x) 'a) (eq (second x) '.$b)))))
(define-test sacla-must-reader.977 (:tag :sacla)
 (assert-true
  (equal (read-from-string "(a b c . d)") (cons 'a (cons 'b (cons 'c 'd))))))
(define-test sacla-must-reader.978 (:tag :sacla)
 (assert-true
  (equal (read-from-string "(this-one . that-one)")
         (cons 'this-one 'that-one))))
(define-test sacla-must-reader.979 (:tag :sacla)
 (assert-true
  (equal (read-from-string "(a b c d . (e f . (g)))") '(a b c d e f g))))
(define-test sacla-must-reader.980 (:tag :sacla)
 (assert-true
  (equal
   (read-from-string
    "(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30)")
   '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27
     28 29 30))))
(define-test sacla-must-reader.981 (:tag :sacla)
 (assert-true
  (handler-case (read-from-string ")")
    (error nil t)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-reader.982 (:tag :sacla)
 (assert-true (equal (read-from-string "(a (b (c d)))") '(a (b (c d))))))
(define-test sacla-must-reader.983 (:tag :sacla)
 (assert-true (equal (read-from-string "'a") ''a)))
(define-test sacla-must-reader.984 (:tag :sacla)
 (assert-true (equal (read-from-string "'(a b c)") ''(a b c))))
(define-test sacla-must-reader.985 (:tag :sacla)
 (assert-true (equal (read-from-string "'''(a b c)") ''''(a b c))))
(define-test sacla-must-reader.986 (:tag :sacla)
 (assert-true (equal (read-from-string "'(a 'b '('c))") ''(a 'b '('c)))))
(define-test sacla-must-reader.987 (:tag :sacla)
 (assert-true
  (equal (read-from-string "'('('a '('b 'c)))") ''('('a '('b 'c))))))
(define-test sacla-must-reader.988 (:tag :sacla)
 (assert-true (equal (read-from-string "''''''a") '''''''a)))
(define-test sacla-must-reader.989 (:tag :sacla)
 (assert-true (equal (read-from-string "' a") ''a)))
(define-test sacla-must-reader.990 (:tag :sacla)
 (assert-true (eq 'quote (eval (read-from-string "(car ''foo)")))))
(define-test sacla-must-reader.991 (:tag :sacla)
 (assert-true
  (eq
   (read-from-string "; comment
a")
   'a)))
(define-test sacla-must-reader.992 (:tag :sacla)
 (assert-true
  (= 7
     (eval
      (read-from-string "(+ 3 ; three
4)")))))
(define-test sacla-must-reader.993 (:tag :sacla)
 (assert-true
  (eq 'a
      (read-from-string ";;;;;;;
a"))))
(define-test sacla-must-reader.994 (:tag :sacla)
 (assert-true
  (equal
   (read-from-string "(a ;;;;;;;
b ;;
;;
c;;;;;;;;;;;;;;;;;;;;;;;;;;;
d)")
   '(a b c d))))
(define-test sacla-must-reader.995 (:tag :sacla)
 (assert-true
  (equal
   (read-from-string "(a ; comment
                                     ;
                                     ;
;
b)")
   '(a b))))
(define-test sacla-must-reader.996 (:tag :sacla)
 (assert-true (equal (read-from-string "(a\\;b c)") '(|A;B| c))))
(define-test sacla-must-reader.997 (:tag :sacla)
 (assert-true (string= (read-from-string "\"hello\"") "hello")))
(define-test sacla-must-reader.998 (:tag :sacla)
 (assert-true (string= (read-from-string "\"\\\"hello\\\"\"") "\"hello\"")))
(define-test sacla-must-reader.999 (:tag :sacla)
 (assert-true (string= (read-from-string "\"|hello|\"") "|hello|")))
(define-test sacla-must-reader.1000 (:tag :sacla)
 (assert-true (string= "string" (read-from-string "  \"string\""))))
(define-test sacla-must-reader.1001 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "\"\\\\\"")))
    (and (= 1 (length x)) (char= #\\ (char x 0))))))
(define-test sacla-must-reader.1002 (:tag :sacla)
 (assert-true
  (string= " This is a sentence. "
           (read-from-string "\" This is a sentence. \""))))
(define-test sacla-must-reader.1003 (:tag :sacla)
 (assert-true (simple-string-p (read-from-string "\"a simple string\""))))
(define-test sacla-must-reader.1004 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "\"\\\"\"")))
    (and (= 1 (length x)) (char= #\" (char x 0))))))
(define-test sacla-must-reader.1005 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "\"|\"")))
    (and (= 1 (length x)) (char= #\| (char x 0))))))
(define-test sacla-must-reader.1006 (:tag :sacla)
 (assert-true (eq (eval (read-from-string "`a")) 'a)))
(define-test sacla-must-reader.1007 (:tag :sacla)
 (assert-true (equal (eval (read-from-string "(let ((x 1)) `(a ,x))")) '(a 1))))
(define-test sacla-must-reader.1008 (:tag :sacla)
 (assert-true
  (equal (eval (read-from-string "(let ((x 1)) `(a ,`(,x)))")) '(a (1)))))
(define-test sacla-must-reader.1009 (:tag :sacla)
 (assert-true
  (equal
   (eval (read-from-string "(let ((a 0) (c 2) (d '(3))) `((,a b) ,c ,@d))"))
   '((0 b) 2 3))))
(define-test sacla-must-reader.1010 (:tag :sacla)
 (assert-true
  (equal
   (eval
    (read-from-string "(let ((a 0) (c 2) (d '(3 4 5))) `((,a b) ,c ,@d))"))
   '((0 b) 2 3 4 5))))
(define-test sacla-must-reader.1011 (:tag :sacla)
 (assert-true
  (equal
   (eval
    (read-from-string "(let ((a '(0 1)) (c 2) (d '(3 4 5)))
 `((,a b) ,c ,@d))"))
   '(((0 1) b) 2 3 4 5))))
(define-test sacla-must-reader.1012 (:tag :sacla)
 (assert-true
  (equal
   (eval
    (read-from-string "(let ((a '(0 1)) (c 2) (d '(3 4 5)))
 `((,@a b) ,c ,@d))"))
   '((0 1 b) 2 3 4 5))))
(define-test sacla-must-reader.1013 (:tag :sacla)
 (assert-true (equal (eval (read-from-string "`(a b ,`c)")) '(a b c))))
(define-test sacla-must-reader.1014 (:tag :sacla)
 (assert-true
  (equal
   (eval (read-from-string "`(a ,@(map 'list #'char-upcase \"bcd\") e f)"))
   '(a #\B #\C #\D e f))))
(define-test sacla-must-reader.1015 (:tag :sacla)
 (assert-true
  (equal (eval (read-from-string "(let ((x 1)) `(a . ,x))")) '(a . 1))))
(define-test sacla-must-reader.1016 (:tag :sacla)
 (assert-true
  (equal (eval (read-from-string "(let ((x '(b c))) `(a . ,x))")) '(a b c))))
(define-test sacla-must-reader.1017 (:tag :sacla)
 (assert-true
  (equalp (eval (read-from-string "(let ((x #(b c))) `(a . ,x))"))
          '(a . #(b c)))))
(define-test sacla-must-reader.1018 (:tag :sacla)
 (assert-true
  (equalp (eval (read-from-string "(let ((x '(b c))) `#(a ,x))")) #(a (b c)))))
(define-test sacla-must-reader.1019 (:tag :sacla)
 (assert-true
  (equalp (eval (read-from-string "(let ((x 'b ) (y 'c)) `#(a ,x ,y))"))
          #(a b c))))
(define-test sacla-must-reader.1020 (:tag :sacla)
 (assert-true
  (equalp (eval (read-from-string "(let ((x '(b c))) `#(a ,@x))")) #(a b c))))
(define-test sacla-must-reader.1021 (:tag :sacla)
 (assert-true (equalp (eval (read-from-string "`\"abc\"")) "abc")))
(define-test sacla-must-reader.1022 (:tag :sacla)
 (assert-true
  (equalp
   (eval
    (read-from-string
     "(let ((x '(b c)) (y '(d e)) (z '(f g))) `(a ,@x ,@y ,@z))"))
   '(a b c d e f g))))
(define-test sacla-must-reader.1023 (:tag :sacla)
 (assert-true
  (equalp
   (eval
    (read-from-string
     "(let ((x '(b c)) (y 'd) (z '(e f g h))) `(a ,@x ,y ,@z))"))
   '(a b c d e f g h))))
(define-test sacla-must-reader.1024 (:tag :sacla)
 (assert-true
  (equal
   (eval
    (read-from-string
     "`(a ,@(mapcar #'char-downcase `(,(char-upcase #\\b) ,(char-upcase #\\c) ,(char-upcase #\\d))) e f)"))
   '(a #\b #\c #\d e f))))
(define-test sacla-must-reader.1025 (:tag :sacla)
 (assert-true
  (equal
   (eval
    (read-from-string
     "`(a ,@(map 'list #'char-downcase `#(,(char-upcase #\\b) ,(char-upcase #\\c) ,(char-upcase #\\d))) e f)"))
   '(a #\b #\c #\d e f))))
(define-test sacla-must-reader.1026 (:tag :sacla)
 (assert-true
  (equal (eval (read-from-string "(let ((x 1)) `(a (,x)))")) '(a (1)))))
(define-test sacla-must-reader.1027 (:tag :sacla)
 (assert-true
  (equal (eval (read-from-string "(let ((x 1)) `(a ((,x))))")) '(a ((1))))))
(define-test sacla-must-reader.1028 (:tag :sacla)
 (assert-true
  (equal (eval (read-from-string "(let ((x 1)) `(a (((,x)))))")) '(a (((1)))))))
(define-test sacla-must-reader.1029 (:tag :sacla)
 (assert-true
  (equalp (eval (read-from-string "(let ((x 1)) `(a ((#(,x)))))"))
          '(a ((#(1)))))))
(define-test sacla-must-reader.1030 (:tag :sacla)
 (assert-true
  (equalp (eval (read-from-string "(let ((x 1)) `(a #((#(,x)))))"))
          '(a #((#(1)))))))
(define-test sacla-must-reader.1031 (:tag :sacla)
 (assert-true
  (equalp (eval (read-from-string "(let ((x 1)) `#(a #((#(,x)))))"))
          '#(a #((#(1)))))))
(define-test sacla-must-reader.1032 (:tag :sacla)
 (assert-true
  (equal
   (eval (read-from-string "(let ((x 1) (y 2) (z 3)) `(,x (,y) ((,z))))"))
   '(1 (2) ((3))))))
(define-test sacla-must-reader.1033 (:tag :sacla)
 (assert-true
  (equal
   (eval
    (read-from-string "(let ((x 1) (y 2) (z 3)) `((,x) ((,y)) (((,z)))))"))
   '((1) ((2)) (((3)))))))
(define-test sacla-must-reader.1034 (:tag :sacla)
 (assert-true
  (equal
   (eval
    (read-from-string
     "(let ((x 1) (y 2) (z 3)) `(((,x)) (((,y))) ((((,z))))))"))
   '(((1)) (((2))) ((((3))))))))
(define-test sacla-must-reader.1035 (:tag :sacla)
 (assert-true
  (equal
   (eval
    (read-from-string
     "(let ((x 1) (y 2) (z 3)) `((((,x))) ((((,y)))) (((((,z)))))))"))
   '((((1))) ((((2)))) (((((3)))))))))
(define-test sacla-must-reader.1036 (:tag :sacla)
 (assert-true
  (equalp
   (eval (read-from-string "(let ((x 1) (y 2) (z 3)) `#(,x (,y) ((,z))))"))
   '#(1 (2) ((3))))))
(define-test sacla-must-reader.1037 (:tag :sacla)
 (assert-true
  (equalp
   (eval
    (read-from-string "(let ((x 1) (y 2) (z 3)) `#((,x) ((,y)) (((,z)))))"))
   '#((1) ((2)) (((3)))))))
(define-test sacla-must-reader.1038 (:tag :sacla)
 (assert-true
  (equalp
   (eval
    (read-from-string
     "(let ((x 1) (y 2) (z 3)) `#(((,x)) (((,y))) ((((,z))))))"))
   '#(((1)) (((2))) ((((3))))))))
(define-test sacla-must-reader.1039 (:tag :sacla)
 (assert-true
  (equalp
   (eval
    (read-from-string
     "(let ((x 1) (y 2) (z 3)) `#((((,x))) ((((,y)))) (((((,z)))))))"))
   '#((((1))) ((((2)))) (((((3)))))))))
(define-test sacla-must-reader.1040 (:tag :sacla)
 (assert-true (equal (eval (read-from-string "(let ((x 1)) `'(,x))")) ''(1))))
(define-test sacla-must-reader.1041 (:tag :sacla)
 (assert-true (equal (eval (read-from-string "(let ((x 1)) `'(',x))")) ''('1))))
(define-test sacla-must-reader.1042 (:tag :sacla)
 (assert-true (equal (eval (read-from-string "`'(','x))")) ''('x))))
(define-test sacla-must-reader.1043 (:tag :sacla)
 (assert-true (equal (eval (read-from-string "`(a . b)")) '(a . b))))
(define-test sacla-must-reader.1044 (:tag :sacla)
 (assert-true
  (equal (eval (read-from-string "(let ((x 1)) `(a . ,x))")) '(a . 1))))
(define-test sacla-must-reader.1045 (:tag :sacla)
 (assert-true
  (equal (eval (read-from-string "(let ((x 1)) `(a . (b . (,x))))")) '(a b 1))))
(define-test sacla-must-reader.1046 (:tag :sacla)
 (assert-true
  (equal (eval (read-from-string "(let ((x 1)) `(a ,x . z))")) '(a 1 . z))))
(define-test sacla-must-reader.1047 (:tag :sacla)
 (assert-true
  (equalp (eval (read-from-string "(let ((x 1)) `(a #(#(#(,x))) . z))"))
          '(a #(#(#(1))) . z))))
(define-test sacla-must-reader.1048 (:tag :sacla)
 (assert-true
  (handler-case (read-from-string ",")
    (error nil t)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-reader.1049 (:tag :sacla)
 (assert-true
  (handler-case (read-from-string "'(,x)")
    (error nil t)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-reader.1050 (:tag :sacla)
 (assert-true
  (handler-case (read-from-string "`(,(append ,x y))")
    (error nil t)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-reader.1051 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\a") #\a)))
(define-test sacla-must-reader.1052 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\b") #\b)))
(define-test sacla-must-reader.1053 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\c") #\c)))
(define-test sacla-must-reader.1054 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\d") #\d)))
(define-test sacla-must-reader.1055 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\e") #\e)))
(define-test sacla-must-reader.1056 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\f") #\f)))
(define-test sacla-must-reader.1057 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\g") #\g)))
(define-test sacla-must-reader.1058 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\h") #\h)))
(define-test sacla-must-reader.1059 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\i") #\i)))
(define-test sacla-must-reader.1060 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\j") #\j)))
(define-test sacla-must-reader.1061 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\k") #\k)))
(define-test sacla-must-reader.1062 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\l") #\l)))
(define-test sacla-must-reader.1063 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\m") #\m)))
(define-test sacla-must-reader.1064 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\n") #\n)))
(define-test sacla-must-reader.1065 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\o") #\o)))
(define-test sacla-must-reader.1066 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\p") #\p)))
(define-test sacla-must-reader.1067 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\q") #\q)))
(define-test sacla-must-reader.1068 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\r") #\r)))
(define-test sacla-must-reader.1069 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\s") #\s)))
(define-test sacla-must-reader.1070 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\t") #\t)))
(define-test sacla-must-reader.1071 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\u") #\u)))
(define-test sacla-must-reader.1072 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\v") #\v)))
(define-test sacla-must-reader.1073 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\w") #\w)))
(define-test sacla-must-reader.1074 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\x") #\x)))
(define-test sacla-must-reader.1075 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\y") #\y)))
(define-test sacla-must-reader.1076 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\z") #\z)))
(define-test sacla-must-reader.1077 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\A") #\A)))
(define-test sacla-must-reader.1078 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\B") #\B)))
(define-test sacla-must-reader.1079 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\C") #\C)))
(define-test sacla-must-reader.1080 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\D") #\D)))
(define-test sacla-must-reader.1081 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\E") #\E)))
(define-test sacla-must-reader.1082 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\F") #\F)))
(define-test sacla-must-reader.1083 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\G") #\G)))
(define-test sacla-must-reader.1084 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\H") #\H)))
(define-test sacla-must-reader.1085 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\I") #\I)))
(define-test sacla-must-reader.1086 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\J") #\J)))
(define-test sacla-must-reader.1087 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\K") #\K)))
(define-test sacla-must-reader.1088 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\L") #\L)))
(define-test sacla-must-reader.1089 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\M") #\M)))
(define-test sacla-must-reader.1090 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\N") #\N)))
(define-test sacla-must-reader.1091 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\O") #\O)))
(define-test sacla-must-reader.1092 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\P") #\P)))
(define-test sacla-must-reader.1093 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\Q") #\Q)))
(define-test sacla-must-reader.1094 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\R") #\R)))
(define-test sacla-must-reader.1095 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\S") #\S)))
(define-test sacla-must-reader.1096 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\T") #\T)))
(define-test sacla-must-reader.1097 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\U") #\U)))
(define-test sacla-must-reader.1098 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\V") #\V)))
(define-test sacla-must-reader.1099 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\W") #\W)))
(define-test sacla-must-reader.1100 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\X") #\X)))
(define-test sacla-must-reader.1101 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\Y") #\Y)))
(define-test sacla-must-reader.1102 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\Z") #\Z)))
(define-test sacla-must-reader.1103 (:tag :sacla)
 (assert-true
  (not (char= (read-from-string "#\\Z") (read-from-string "#\\z")))))
(define-test sacla-must-reader.1104 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\0") #\0)))
(define-test sacla-must-reader.1105 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\1") #\1)))
(define-test sacla-must-reader.1106 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\2") #\2)))
(define-test sacla-must-reader.1107 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\3") #\3)))
(define-test sacla-must-reader.1108 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\4") #\4)))
(define-test sacla-must-reader.1109 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\5") #\5)))
(define-test sacla-must-reader.1110 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\6") #\6)))
(define-test sacla-must-reader.1111 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\7") #\7)))
(define-test sacla-must-reader.1112 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\8") #\8)))
(define-test sacla-must-reader.1113 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\9") #\9)))
(define-test sacla-must-reader.1114 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\!") #\!)))
(define-test sacla-must-reader.1115 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\$") #\$)))
(define-test sacla-must-reader.1116 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\\"") #\")))
(define-test sacla-must-reader.1117 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\'") #\')))
(define-test sacla-must-reader.1118 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\(") #\()))
(define-test sacla-must-reader.1119 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\)") #\))))
(define-test sacla-must-reader.1120 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\,") #\,)))
(define-test sacla-must-reader.1121 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\_") #\_)))
(define-test sacla-must-reader.1122 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\-") #\-)))
(define-test sacla-must-reader.1123 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\.") #\.)))
(define-test sacla-must-reader.1124 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\/") #\/)))
(define-test sacla-must-reader.1125 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\:") #\:)))
(define-test sacla-must-reader.1126 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\;") #\;)))
(define-test sacla-must-reader.1127 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\?") #\?)))
(define-test sacla-must-reader.1128 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\+") #\+)))
(define-test sacla-must-reader.1129 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\<") #\<)))
(define-test sacla-must-reader.1130 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\=") #\=)))
(define-test sacla-must-reader.1131 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\>") #\>)))
(define-test sacla-must-reader.1132 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\#") #\#)))
(define-test sacla-must-reader.1133 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\%") #\%)))
(define-test sacla-must-reader.1134 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\&") #\&)))
(define-test sacla-must-reader.1135 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\*") #\*)))
(define-test sacla-must-reader.1136 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\@") #\@)))
(define-test sacla-must-reader.1137 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\[") #\[)))
(define-test sacla-must-reader.1138 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\\\") #\\)))
(define-test sacla-must-reader.1139 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\]") #\])))
(define-test sacla-must-reader.1140 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\{") #\{)))
(define-test sacla-must-reader.1141 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\|") #\|)))
(define-test sacla-must-reader.1142 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\}") #\})))
(define-test sacla-must-reader.1143 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\`") #\`)))
(define-test sacla-must-reader.1144 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\^") #\^)))
(define-test sacla-must-reader.1145 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\~") #\~)))
(define-test sacla-must-reader.1146 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\newline") #\Newline)))
(define-test sacla-must-reader.1147 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\space") #\ )))
(define-test sacla-must-reader.1148 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\Newline") #\Newline)))
(define-test sacla-must-reader.1149 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\Space") #\ )))
(define-test sacla-must-reader.1150 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\NeWlInE") #\Newline)))
(define-test sacla-must-reader.1151 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\SpAcE") #\ )))
(define-test sacla-must-reader.1152 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\NEWLINE") #\Newline)))
(define-test sacla-must-reader.1153 (:tag :sacla)
 (assert-true (char= (read-from-string "#\\SPACE") #\ )))
(define-test sacla-must-reader.1154 (:tag :sacla)
 (assert-true (equal (read-from-string "#'car") '#'car)))
(define-test sacla-must-reader.1155 (:tag :sacla)
 (assert-true (eq (eval (read-from-string "#'car")) #'car)))
(define-test sacla-must-reader.1156 (:tag :sacla)
 (assert-true (simple-vector-p (read-from-string "#(a)"))))
(define-test sacla-must-reader.1157 (:tag :sacla)
 (assert-true (equalp (read-from-string "#(a)") #(a))))
(define-test sacla-must-reader.1158 (:tag :sacla)
 (assert-true (equalp (read-from-string "#()") #())))
(define-test sacla-must-reader.1159 (:tag :sacla)
 (assert-true (equalp (read-from-string "#(a b)") #(a b))))
(define-test sacla-must-reader.1160 (:tag :sacla)
 (assert-true (equalp (read-from-string "#(a b c)") #(a b c))))
(define-test sacla-must-reader.1161 (:tag :sacla)
 (assert-true (equalp (read-from-string "#(a b c d)") #(a b c d))))
(define-test sacla-must-reader.1162 (:tag :sacla)
 (assert-true (equalp (read-from-string "#(a b c d e)") #(a b c d e))))
(define-test sacla-must-reader.1163 (:tag :sacla)
 (assert-true (equalp (read-from-string "#(a b c d e f)") #(a b c d e f))))
(define-test sacla-must-reader.1164 (:tag :sacla)
 (assert-true (equalp (read-from-string "#(a b c d e f g)") #(a b c d e f g))))
(define-test sacla-must-reader.1165 (:tag :sacla)
 (assert-true (equalp (read-from-string "#(a b c c c c)") #(a b c c c c))))
(define-test sacla-must-reader.1166 (:tag :sacla)
 (assert-true (equalp (read-from-string "#6(a b c c c c)") #(a b c c c c))))
(define-test sacla-must-reader.1167 (:tag :sacla)
 (assert-true (equalp (read-from-string "#6(a b c)") #(a b c c c c))))
(define-test sacla-must-reader.1168 (:tag :sacla)
 (assert-true (equalp (read-from-string "#6(a b c c)") #(a b c c c c))))
(define-test sacla-must-reader.1169 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#(a b c)")))
    (= 3 (length x)))))
(define-test sacla-must-reader.1170 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#()")))
    (and (simple-vector-p x) (zerop (length x)) (equalp x #())))))
(define-test sacla-must-reader.1171 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#0()")))
    (and (simple-vector-p x) (zerop (length x)) (equalp x #())))))
(define-test sacla-must-reader.1172 (:tag :sacla)
 (assert-true (equalp (read-from-string "#1(a)") #(a))))
(define-test sacla-must-reader.1173 (:tag :sacla)
 (assert-true (equalp (read-from-string "#2(a b)") #(a b))))
(define-test sacla-must-reader.1174 (:tag :sacla)
 (assert-true (equalp (read-from-string "#3(a b c)") #(a b c))))
(define-test sacla-must-reader.1175 (:tag :sacla)
 (assert-true (equalp (read-from-string "#4(a b c d)") #(a b c d))))
(define-test sacla-must-reader.1176 (:tag :sacla)
 (assert-true (equalp (read-from-string "#5(a b c d e)") #(a b c d e))))
(define-test sacla-must-reader.1177 (:tag :sacla)
 (assert-true (equalp (read-from-string "#6(a b c d e f)") #(a b c d e f))))
(define-test sacla-must-reader.1178 (:tag :sacla)
 (assert-true (equalp (read-from-string "#2(a)") #(a a))))
(define-test sacla-must-reader.1179 (:tag :sacla)
 (assert-true (equalp (read-from-string "#3(a)") #(a a a))))
(define-test sacla-must-reader.1180 (:tag :sacla)
 (assert-true (equalp (read-from-string "#4(a)") #(a a a a))))
(define-test sacla-must-reader.1181 (:tag :sacla)
 (assert-true (equalp (read-from-string "#5(a)") #(a a a a a))))
(define-test sacla-must-reader.1182 (:tag :sacla)
 (assert-true (equalp (read-from-string "#6(a)") #(a a a a a a))))
(define-test sacla-must-reader.1183 (:tag :sacla)
 (assert-true (equalp (read-from-string "#7(a)") #(a a a a a a a))))
(define-test sacla-must-reader.1184 (:tag :sacla)
 (assert-true (equalp (read-from-string "#8(a)") #(a a a a a a a a))))
(define-test sacla-must-reader.1185 (:tag :sacla)
 (assert-true (equalp (read-from-string "#9(a)") #(a a a a a a a a a))))
(define-test sacla-must-reader.1186 (:tag :sacla)
 (assert-true (equalp (read-from-string "#10(a)") #(a a a a a a a a a a))))
(define-test sacla-must-reader.1187 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#100(a)")))
    (and (simple-vector-p x)
         (= 100 (length x))
         (every #'symbolp x)
         (every #'(lambda (s) (eq s 'a)) x)))))
(define-test sacla-must-reader.1188 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#100(#\\z)")))
    (and (simple-vector-p x)
         (= 100 (length x))
         (every #'characterp x)
         (every #'(lambda (c) (char= c #\z)) x)))))
(define-test sacla-must-reader.1189 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#100(#())")))
    (and (simple-vector-p x)
         (= 100 (length x))
         (every #'simple-vector-p x)
         (every #'(lambda (v) (zerop (length v))) x)))))
(define-test sacla-must-reader.1190 (:tag :sacla)
 (assert-true (equalp (read-from-string "#*0") #*0)))
(define-test sacla-must-reader.1191 (:tag :sacla)
 (assert-true (equalp (read-from-string "#*1") #*1)))
(define-test sacla-must-reader.1192 (:tag :sacla)
 (assert-true (equalp (read-from-string "#*01") #*01)))
(define-test sacla-must-reader.1193 (:tag :sacla)
 (assert-true (equalp (read-from-string "#*10") #*10)))
(define-test sacla-must-reader.1194 (:tag :sacla)
 (assert-true (equalp (read-from-string "#*11") #*11)))
(define-test sacla-must-reader.1195 (:tag :sacla)
 (assert-true (equalp (read-from-string "#0*") #*)))
(define-test sacla-must-reader.1196 (:tag :sacla)
 (assert-true (equalp (read-from-string "#*") #*)))
(define-test sacla-must-reader.1197 (:tag :sacla)
 (assert-true (equalp (read-from-string "#3*1") #*111)))
(define-test sacla-must-reader.1198 (:tag :sacla)
 (assert-true (equalp (read-from-string "#3*10") #*100)))
(define-test sacla-must-reader.1199 (:tag :sacla)
 (assert-true (equalp (read-from-string "#*101111") #*101111)))
(define-test sacla-must-reader.1200 (:tag :sacla)
 (assert-true (equalp (read-from-string "#6*101111") #*101111)))
(define-test sacla-must-reader.1201 (:tag :sacla)
 (assert-true (equalp (read-from-string "#6*101") #*101111)))
(define-test sacla-must-reader.1202 (:tag :sacla)
 (assert-true (equalp (read-from-string "#6*1011") #*101111)))
(define-test sacla-must-reader.1203 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#*10")))
    (and (simple-bit-vector-p x)
         (= 2 (length x))
         (= 1 (bit x 0))
         (= 0 (bit x 1))))))
(define-test sacla-must-reader.1204 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#*")))
    (and (simple-bit-vector-p x) (zerop (length x))))))
(define-test sacla-must-reader.1205 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#100*0")))
    (and (simple-bit-vector-p x) (= 100 (length x)) (every #'zerop x)))))
(define-test sacla-must-reader.1206 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#100*1")))
    (and (simple-bit-vector-p x)
         (= 100 (length x))
         (every #'(lambda (n) (= 1 n)) x)))))
(define-test sacla-must-reader.1207 (:tag :sacla)
 (assert-true
  (handler-case (read-from-string "#3*1110")
    (reader-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-reader.1208 (:tag :sacla)
 (assert-true
  (handler-case (read-from-string "#3*")
    (reader-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-reader.1209 (:tag :sacla)
 (assert-true
  (handler-case (read-from-string "#3*abc")
    (reader-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-reader.1210 (:tag :sacla)
 (assert-true
  (let ((symbol (read-from-string "#:ok")))
    (and (null (symbol-package symbol)) (string= (symbol-name symbol) "OK")))))
(define-test sacla-must-reader.1211 (:tag :sacla)
 (assert-true
  (let ((symbol (read-from-string "#:g10")))
    (and (null (symbol-package symbol)) (string= (symbol-name symbol) "G10")))))
(define-test sacla-must-reader.1212 (:tag :sacla)
 (assert-true
  (let ((symbol (read-from-string "#:10")))
    (and (null (symbol-package symbol)) (string= (symbol-name symbol) "10")))))
(define-test sacla-must-reader.1213 (:tag :sacla)
 (assert-true
  (let ((symbol (read-from-string "#:0")))
    (and (null (symbol-package symbol)) (string= (symbol-name symbol) "0")))))
(define-test sacla-must-reader.1214 (:tag :sacla)
 (assert-true
  (let ((symbol (read-from-string "#:-")))
    (and (null (symbol-package symbol)) (string= (symbol-name symbol) "-")))))
(define-test sacla-must-reader.1215 (:tag :sacla)
 (assert-true
  (let ((symbol (read-from-string "#:\\-")))
    (and (null (symbol-package symbol)) (string= (symbol-name symbol) "-")))))
(define-test sacla-must-reader.1216 (:tag :sacla)
 (assert-true
  (let ((symbol (read-from-string "#:$$-$$")))
    (and (null (symbol-package symbol))
         (string= (symbol-name symbol) "$$-$$")))))
(define-test sacla-must-reader.1217 (:tag :sacla)
 (assert-true (eq 'a (read-from-string "#.'a"))))
(define-test sacla-must-reader.1218 (:tag :sacla)
 (assert-true (packagep (read-from-string "#.*package*"))))
(define-test sacla-must-reader.1219 (:tag :sacla)
 (assert-true (= 11 (read-from-string "#.(let ((x 10)) (1+ x))"))))
(define-test sacla-must-reader.1220 (:tag :sacla)
 (assert-true (= 4 (read-from-string "#.(1+ 3)"))))
(define-test sacla-must-reader.1221 (:tag :sacla)
 (assert-true
  (handler-case
      (let ((*read-eval* nil))
        (read-from-string "#.(1+ 3)"))
    (reader-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-reader.1222 (:tag :sacla)
 (assert-true
  (equal '(a b . 3) (read-from-string "#.(let ((x 3)) `(a b . ,x))"))))
(define-test sacla-must-reader.1223 (:tag :sacla)
 (assert-true (= (read-from-string "#b0") 0)))
(define-test sacla-must-reader.1224 (:tag :sacla)
 (assert-true (= (read-from-string "#B0") 0)))
(define-test sacla-must-reader.1225 (:tag :sacla)
 (assert-true (= (read-from-string "#b01") 1)))
(define-test sacla-must-reader.1226 (:tag :sacla)
 (assert-true (= (read-from-string "#B01") 1)))
(define-test sacla-must-reader.1227 (:tag :sacla)
 (assert-true (= (read-from-string "#B1101") 13)))
(define-test sacla-must-reader.1228 (:tag :sacla)
 (assert-true (= (read-from-string "#b101/11") 5/3)))
(define-test sacla-must-reader.1229 (:tag :sacla)
 (assert-true
  (= 172236929 (read-from-string "#b1010010001000010000010000001"))))
(define-test sacla-must-reader.1230 (:tag :sacla)
 (assert-true (= (read-from-string "#o0") 0)))
(define-test sacla-must-reader.1231 (:tag :sacla)
 (assert-true (= (read-from-string "#O0") 0)))
(define-test sacla-must-reader.1232 (:tag :sacla)
 (assert-true (= (read-from-string "#o37/15") 31/13)))
(define-test sacla-must-reader.1233 (:tag :sacla)
 (assert-true (= (read-from-string "#o777") 511)))
(define-test sacla-must-reader.1234 (:tag :sacla)
 (assert-true (= (read-from-string "#o105") 69)))
(define-test sacla-must-reader.1235 (:tag :sacla)
 (assert-true (= (read-from-string "#O37/15") 31/13)))
(define-test sacla-must-reader.1236 (:tag :sacla)
 (assert-true (= (read-from-string "#O777") 511)))
(define-test sacla-must-reader.1237 (:tag :sacla)
 (assert-true (= (read-from-string "#O105") 69)))
(define-test sacla-must-reader.1238 (:tag :sacla)
 (assert-true (= 342391 (read-from-string "#o1234567"))))
(define-test sacla-must-reader.1239 (:tag :sacla)
 (assert-true (= (read-from-string "#x0") 0)))
(define-test sacla-must-reader.1240 (:tag :sacla)
 (assert-true (= (read-from-string "#xF00") 3840)))
(define-test sacla-must-reader.1241 (:tag :sacla)
 (assert-true (= (read-from-string "#x105") 261)))
(define-test sacla-must-reader.1242 (:tag :sacla)
 (assert-true (= (read-from-string "#X0") 0)))
(define-test sacla-must-reader.1243 (:tag :sacla)
 (assert-true (= (read-from-string "#XF00") 3840)))
(define-test sacla-must-reader.1244 (:tag :sacla)
 (assert-true (= (read-from-string "#Xf00") 3840)))
(define-test sacla-must-reader.1245 (:tag :sacla)
 (assert-true (= (read-from-string "#X105") 261)))
(define-test sacla-must-reader.1246 (:tag :sacla)
 (assert-true (= 81985529216486895 (read-from-string "#X0123456789ABCDEF"))))
(define-test sacla-must-reader.1247 (:tag :sacla)
 (assert-true (= (read-from-string "#3r0") 0)))
(define-test sacla-must-reader.1248 (:tag :sacla)
 (assert-true (= (read-from-string "#2r11010101") 213)))
(define-test sacla-must-reader.1249 (:tag :sacla)
 (assert-true (= (read-from-string "#b11010101") 213)))
(define-test sacla-must-reader.1250 (:tag :sacla)
 (assert-true (= (read-from-string "#b+11010101") 213)))
(define-test sacla-must-reader.1251 (:tag :sacla)
 (assert-true (= (read-from-string "#o325") 213)))
(define-test sacla-must-reader.1252 (:tag :sacla)
 (assert-true (= (read-from-string "#xD5") 213)))
(define-test sacla-must-reader.1253 (:tag :sacla)
 (assert-true (= (read-from-string "#16r+D5") 213)))
(define-test sacla-must-reader.1254 (:tag :sacla)
 (assert-true (= (read-from-string "#o-300") -192)))
(define-test sacla-must-reader.1255 (:tag :sacla)
 (assert-true (= (read-from-string "#3r-21010") -192)))
(define-test sacla-must-reader.1256 (:tag :sacla)
 (assert-true (= (read-from-string "#25R-7H") -192)))
(define-test sacla-must-reader.1257 (:tag :sacla)
 (assert-true (= (read-from-string "#xACCEDED") 181202413)))
(define-test sacla-must-reader.1258 (:tag :sacla)
 (assert-true (zerop (read-from-string "#c(0 0)"))))
(define-test sacla-must-reader.1259 (:tag :sacla)
 (assert-true (= (read-from-string "#c(1 0)") 1)))
(define-test sacla-must-reader.1260 (:tag :sacla)
 (assert-true (complexp (read-from-string "#c(1 10)"))))
(define-test sacla-must-reader.1261 (:tag :sacla)
 (assert-true (= (read-from-string "#c(1 0)") 1)))
(define-test sacla-must-reader.1262 (:tag :sacla)
 (assert-true (= (read-from-string "#c(0 1)") #C(0 1))))
(define-test sacla-must-reader.1263 (:tag :sacla)
 (assert-true (= (read-from-string "#c(1 1)") #C(1 1))))
(define-test sacla-must-reader.1264 (:tag :sacla)
 (assert-true (= (read-from-string "#C(3.0s1 2.0s-1)") #C(30.0 0.2))))
(define-test sacla-must-reader.1265 (:tag :sacla)
 (assert-true (= (read-from-string "#C(5 -3)") #C(5 -3))))
(define-test sacla-must-reader.1266 (:tag :sacla)
 (assert-true (= (read-from-string "#C(5/3 7.0)") #C(1.6666666 7.0))))
(define-test sacla-must-reader.1267 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#C(5/3 7.0)")))
    (and (floatp (realpart x)) (floatp (imagpart x))))))
(define-test sacla-must-reader.1268 (:tag :sacla)
 (assert-true (= (read-from-string "#C(0 1)") #C(0 1))))
(define-test sacla-must-reader.1269 (:tag :sacla)
 (assert-true (equalp (read-from-string "#1A(0 1)") #(0 1))))
(define-test sacla-must-reader.1270 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#1A(0 1)")))
    (and (vectorp x) (= 2 (length x)) (= 0 (aref x 0)) (= 1 (aref x 1))))))
(define-test sacla-must-reader.1271 (:tag :sacla)
 (assert-true
  (equalp (read-from-string "#2A((0 1 5) (foo 2 (hot dog)))")
          #2A((0 1 5) (foo 2 (hot dog))))))
(define-test sacla-must-reader.1272 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#2A((0 1 5) (foo 2 (hot dog)))")))
    (and (arrayp x)
         (equal (array-dimensions x) '(2 3))
         (zerop (aref x 0 0))
         (= (aref x 0 1) 1)
         (= (aref x 0 2) 5)
         (eq (aref x 1 0) 'foo)
         (= (aref x 1 1) 2)
         (equal (aref x 1 2) '(hot dog))))))
(define-test sacla-must-reader.1273 (:tag :sacla)
 (assert-true
  (equal (aref (read-from-string "#0A((0 1 5) (foo 2 (hot dog)))"))
         '((0 1 5) (foo 2 (hot dog))))))
(define-test sacla-must-reader.1274 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#0A((0 1 5) (foo 2 (hot dog)))")))
    (and (arrayp x)
         (null (array-dimensions x))
         (equal (aref x) '((0 1 5) (foo 2 (hot dog))))))))
(define-test sacla-must-reader.1275 (:tag :sacla)
 (assert-true (equalp (read-from-string "#0A foo") #0Afoo)))
(define-test sacla-must-reader.1276 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#0A foo")))
    (and (arrayp x) (null (array-dimensions x)) (eq (aref x) 'foo)))))
(define-test sacla-must-reader.1277 (:tag :sacla)
 (assert-true
  (equal (array-dimensions (read-from-string "#3A((() ()) (() ()) (() ()))"))
         '(3 2 0))))
(define-test sacla-must-reader.1278 (:tag :sacla)
 (assert-true
  (equal (array-dimensions (read-from-string "#10A(() ())"))
         '(2 0 0 0 0 0 0 0 0 0))))
(define-test sacla-must-reader.1279 (:tag :sacla)
 (assert-true
  (let ((x
         (read-from-string "
#4A((((0 1 2 3)     (4 5 6 7)     (8 9 10 11))
     ((12 13 14 15) (16 17 18 19) (20 21 22 23))))")))
    (and (arrayp x)
         (equal (array-dimensions x) '(1 2 3 4))
         (loop for i below 24 always (= i (row-major-aref x i)))))))
(define-test sacla-must-reader.1280 (:tag :sacla)
 (assert-true (eq (read-from-string "#1=a") 'a)))
(define-test sacla-must-reader.1281 (:tag :sacla)
 (assert-true (equal (read-from-string "(#1=a #1#)") '(a a))))
(define-test sacla-must-reader.1282 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#1=(a . #1#)")))
    (eq x (cdr x)))))
(define-test sacla-must-reader.1283 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "((a b) . #1=(#2=(p q) foo #2# . #1#))")))
    (and (eq (nthcdr 1 x) (nthcdr 4 x))
         (eq (nthcdr 4 x) (nthcdr 7 x))
         (eq (nthcdr 7 x) (nthcdr 10 x))
         (eq (nth 1 x) (nth 3 x))
         (eq (nth 3 x) (nth 6 x))
         (eq (nth 6 x) (nth 9 x))
         (eq (nth 9 x) (nth 12 x))))))
(define-test sacla-must-reader.1284 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "(#1=(a . #1#) #2=(#1# . #2#))")))
    (and (eq (car x) (caadr x))
         (eq (car x) (cdar x))
         (eq (cadr x) (cdadr x))))))
(define-test sacla-must-reader.1285 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#1=#2=#3=(0 . #1#)")))
    (and (eq x (cdr x)) (zerop (car x))))))
(define-test sacla-must-reader.1286 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#1=#2=#3=(0 . #2#)")))
    (and (eq x (cdr x)) (zerop (car x))))))
(define-test sacla-must-reader.1287 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#1=#2=#3=(0 . #3#)")))
    (and (eq x (cdr x)) (zerop (car x))))))
(define-test sacla-must-reader.1288 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#1=#2=#3=(0 #1# #2# #3#)")))
    (and (= 4 (length x))
         (zerop (first x))
         (eq x (second x))
         (eq x (third x))
         (eq x (fourth x))))))
(define-test sacla-must-reader.1289 (:tag :sacla)
 (assert-true (equal (read-from-string "(#1000=a #1000#)") '(a a))))
(define-test sacla-must-reader.1290 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "(#1=#:g10 #1#)")))
    (and (= 2 (length x))
         (string= (symbol-name (first x)) "G10")
         (eq (first x) (second x))))))
(define-test sacla-must-reader.1291 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#1=(a (b #2=(x y z) . #1#) . #2#)")))
    (and (eq (first x) 'a)
         (eq x (cddr (second x)))
         (eq (second (second x)) (cddr x))))))
(define-test sacla-must-reader.1292 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "(#1=(a (b #2=(x y z) . #1#) . #2#))")))
    (and (eq (caar x) 'a)
         (eq (car x) (cddr (second (first x))))
         (eq (second (second (first x))) (cddr (first x)))))))
(define-test sacla-must-reader.1293 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#1=(a #2=(b #3=(c . #3#) . #2#) . #1#)")))
    (and (eq (first x) 'a)
         (eq (first (second x)) 'b)
         (eq (first (second (second x))) 'c)
         (eq x (cddr x))
         (eq (second x) (cddr (second x)))
         (eq (second (second x)) (cdr (second (second x))))))))
(define-test sacla-must-reader.1294 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#1=(a #2=(b #3=(c . #1#) . #2#) . #3#)")))
    (and (eq (first x) 'a)
         (eq (first (second x)) 'b)
         (eq (first (second (second x))) 'c)
         (eq x (cdr (second (second x))))
         (eq (second x) (cddr (second x)))
         (eq (second (second x)) (cddr x))))))
(define-test sacla-must-reader.1295 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "(#1=#(0 1 2) #1#)")))
    (and (= 2 (length x))
         (eq (first x) (second x))
         (equalp (first x) #(0 1 2))))))
(define-test sacla-must-reader.1296 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#1=#(#1# 1 2)")))
    (and (= 3 (length x))
         (eq (aref x 0) x)
         (= (aref x 1) 1)
         (= (aref x 2) 2)))))
(define-test sacla-must-reader.1297 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#(#1=#:g00 a b #1#)")))
    (and (= 4 (length x))
         (string= (symbol-name (aref x 0)) "G00")
         (eq (aref x 0) (aref x 3))
         (eq (aref x 1) 'a)
         (eq (aref x 2) 'b)))))
(define-test sacla-must-reader.1298 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#1=#(#2=#:g00 a #2# #1#)")))
    (and (= 4 (length x))
         (string= (symbol-name (aref x 0)) "G00")
         (eq x (aref x 3))
         (eq (aref x 0) (aref x 2))
         (eq (aref x 1) 'a)))))
(define-test sacla-must-reader.1299 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#1=#(#1# #1# #1#)")))
    (and (= 3 (length x))
         (eq x (aref x 0))
         (eq (aref x 0) (aref x 1))
         (eq (aref x 1) (aref x 2))))))
(define-test sacla-must-reader.1300 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#1=#(#(#1#))")))
    (and (= 1 (length x))
         (= 1 (length (aref x 0)))
         (eq x (aref (aref x 0) 0))))))
(define-test sacla-must-reader.1301 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#1=#(#2=#(#3=#(#3# #2# #1#))))")))
    (and (= 1 (length x))
         (= 1 (length (aref x 0)))
         (= 3 (length (aref (aref x 0) 0)))
         (eq x (aref (aref (aref x 0) 0) 2))
         (eq (aref x 0) (aref (aref (aref x 0) 0) 1))
         (eq (aref (aref x 0) 0) (aref (aref (aref x 0) 0) 0))))))
(define-test sacla-must-reader.1302 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#1=#(#2=#(#3=#(#1# #2# #3#))))")))
    (and (= 1 (length x))
         (= 1 (length (aref x 0)))
         (= 3 (length (aref (aref x 0) 0)))
         (eq x (aref (aref (aref x 0) 0) 0))
         (eq (aref x 0) (aref (aref (aref x 0) 0) 1))
         (eq (aref (aref x 0) 0) (aref (aref (aref x 0) 0) 2))))))
(define-test sacla-must-reader.1303 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "(#1=#(0 #2=#:g100 2) #2# #1#)")))
    (and (= 3 (length x))
         (eq (first x) (third x))
         (string= (symbol-name (aref (first x) 1)) "G100")
         (null (symbol-package (aref (first x) 1)))
         (eq (aref (first x) 1) (second x))))))
(define-test sacla-must-reader.1304 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "(a #1=#(0 (#1#) 2) c)")))
    (and (= 3 (length x))
         (eq (first x) 'a)
         (eq (second x) (first (aref (second x) 1)))
         (eq (third x) 'c)
         (= 0 (aref (second x) 0))
         (= 2 (aref (second x) 2))))))
(define-test sacla-must-reader.1305 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#1=#2A((a b) (c #1#))")))
    (and (= 4 (array-total-size x))
         (eq (aref x 0 0) 'a)
         (eq (aref x 0 1) 'b)
         (eq (aref x 1 0) 'c)
         (eq (aref x 1 1) x)))))
(define-test sacla-must-reader.1306 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#2A((#1=#:G10 b) (#1# d))")))
    (and (= 4 (array-total-size x))
         (eq (aref x 0 0) (aref x 1 0))
         (null (symbol-package (aref x 0 0)))
         (string= (symbol-name (aref x 0 0)) "G10")
         (eq (aref x 0 1) 'b)
         (eq (aref x 1 1) 'd)))))
(define-test sacla-must-reader.1307 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#1=#2A((#2=#:GG #1#) (#2# #1#))")))
    (and (= 4 (array-total-size x))
         (eq (aref x 0 0) (aref x 1 0))
         (null (symbol-package (aref x 0 0)))
         (string= "GG" (symbol-name (aref x 0 0)))
         (eq x (aref x 0 1))
         (eq x (aref x 1 1))))))
(define-test sacla-must-reader.1308 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#1=#0A#1#")))
    (and (arrayp x) (eq x (aref x))))))
(define-test sacla-must-reader.1309 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#1=#0A(#1#)")))
    (and (arrayp x)
         (consp (aref x))
         (= 1 (length (aref x)))
         (eq x (first (aref x)))))))
(define-test sacla-must-reader.1310 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#1=#1A(#1#)")))
    (and (vectorp x) (= 1 (length x)) (eq x (aref x 0))))))
(define-test sacla-must-reader.1311 (:tag :sacla)
 (assert-true
  (let ((x (read-from-string "#1=#1A(#2=(a b c) #1# #2#)")))
    (and (vectorp x)
         (= 3 (length x))
         (equal (aref x 0) '(a b c))
         (eq (aref x 0) (aref x 2))
         (eq x (aref x 1))))))
(define-test sacla-must-reader.1312 (:tag :sacla)
 (assert-true
  (let ((x
         (read-from-string "#1=#3A(((0 a) (1 b) (2 c))
                  ((3 d) (4 #2A((41 #2=#(x y z)) (43 #1#))) (5 f))
                  ((6 g) (((#2#)) h) (9 i)))")))
    (and (= 18 (array-total-size x))
         (= 0 (aref x 0 0 0))
         (eq 'a (aref x 0 0 1))
         (= 1 (aref x 0 1 0))
         (eq 'b (aref x 0 1 1))
         (= 2 (aref x 0 2 0))
         (eq 'c (aref x 0 2 1))
         (= 3 (aref x 1 0 0))
         (eq 'd (aref x 1 0 1))
         (= 4 (aref x 1 1 0))
         (= (array-total-size (aref x 1 1 1)) 4)
         (= 41 (aref (aref x 1 1 1) 0 0))
         (equalp (aref (aref x 1 1 1) 0 1) #(x y z))
         (= 43 (aref (aref x 1 1 1) 1 0))
         (eq x (aref (aref x 1 1 1) 1 1))
         (= 5 (aref x 1 2 0))
         (eq 'f (aref x 1 2 1))
         (= 6 (aref x 2 0 0))
         (eq 'g (aref x 2 0 1))
         (eq (caar (aref x 2 1 0)) (aref (aref x 1 1 1) 0 1))
         (eq 'h (aref x 2 1 1))
         (= 9 (aref x 2 2 0))
         (eq 'i (aref x 2 2 1))))))
(define-test sacla-must-reader.1313 (:tag :sacla)
 (assert-true
  (progn
    (handler-case
        (null
         (let ((*features* 'nil))
           (read-from-string "#+test1 a")))
      (error nil nil)))))
(define-test sacla-must-reader.1314 (:tag :sacla)
 (assert-true
  (let ((*features* 'nil))
    (equal
     (with-input-from-string (stream "#+test1 a #-test1 b")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     '(b)))))
(define-test sacla-must-reader.1315 (:tag :sacla)
 (assert-true
  (let ((*features* '(:test1)))
    (equal
     (with-input-from-string (stream "#+test1 a #-test1 b")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     '(a)))))
(define-test sacla-must-reader.1316 (:tag :sacla)
 (assert-true
  (let ((*features* 'nil))
    (equal
     (with-input-from-string (stream "#+(not test1) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     '(eat-this)))))
(define-test sacla-must-reader.1317 (:tag :sacla)
 (assert-true
  (let ((*features* '(:test1)))
    (equal
     (with-input-from-string (stream "#+(not test1) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     'nil))))
(define-test sacla-must-reader.1318 (:tag :sacla)
 (assert-true
  (let ((*features* '(:test1)))
    (equal
     (with-input-from-string (stream "#-(not test1) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     '(eat-this)))))
(define-test sacla-must-reader.1319 (:tag :sacla)
 (assert-true
  (let ((*features* 'nil))
    (equal
     (with-input-from-string (stream "#-(not test1) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     'nil))))
(define-test sacla-must-reader.1320 (:tag :sacla)
 (assert-true
  (let ((*features* '(:test1 :test2)))
    (equal
     (with-input-from-string (stream "#+(and test1 test2) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     '(eat-this)))))
(define-test sacla-must-reader.1321 (:tag :sacla)
 (assert-true
  (let ((*features* '(:test1)))
    (equal
     (with-input-from-string (stream "#+(and test1 test2) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     'nil))))
(define-test sacla-must-reader.1322 (:tag :sacla)
 (assert-true
  (let ((*features* 'nil))
    (equal
     (with-input-from-string (stream "#+(and test1 test2) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     'nil))))
(define-test sacla-must-reader.1323 (:tag :sacla)
 (assert-true
  (let ((*features* 'nil))
    (equal
     (with-input-from-string (stream "#+(or test1 test2) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     'nil))))
(define-test sacla-must-reader.1324 (:tag :sacla)
 (assert-true
  (let ((*features* '(:test1)))
    (equal
     (with-input-from-string (stream "#+(or test1 test2) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     '(eat-this)))))
(define-test sacla-must-reader.1325 (:tag :sacla)
 (assert-true
  (let ((*features* '(:test2)))
    (equal
     (with-input-from-string (stream "#+(or test1 test2) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     '(eat-this)))))
(define-test sacla-must-reader.1326 (:tag :sacla)
 (assert-true
  (let ((*features* '(:test1 :test2)))
    (equal
     (with-input-from-string (stream "#+(or test1 test2) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     '(eat-this)))))
(define-test sacla-must-reader.1327 (:tag :sacla)
 (assert-true
  (let ((*features* '(:test1 :test2 :test3)))
    (equal
     (with-input-from-string (stream "#+(or test1 test2) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     '(eat-this)))))
(define-test sacla-must-reader.1328 (:tag :sacla)
 (assert-true
  (let ((*features* '(:test1 :test2)))
    (equal
     (with-input-from-string (stream "#+(and test1 (not test2)) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     'nil))))
(define-test sacla-must-reader.1329 (:tag :sacla)
 (assert-true
  (let ((*features* 'nil))
    (equal
     (with-input-from-string (stream "#+(and test1 (not test2)) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     'nil))))
(define-test sacla-must-reader.1330 (:tag :sacla)
 (assert-true
  (let ((*features* '(:test1)))
    (equal
     (with-input-from-string (stream "#+(and test1 (not test2)) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     '(eat-this)))))
(define-test sacla-must-reader.1331 (:tag :sacla)
 (assert-true
  (let ((*features* 'nil))
    (equal
     (with-input-from-string
         (stream "#+(or (and test1 (not test2)) test3) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     'nil))))
(define-test sacla-must-reader.1332 (:tag :sacla)
 (assert-true
  (let ((*features* '(:test1)))
    (equal
     (with-input-from-string
         (stream "#+(or (and test1 (not test2)) test3) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     '(eat-this)))))
(define-test sacla-must-reader.1333 (:tag :sacla)
 (assert-true
  (let ((*features* '(:test1 :test2)))
    (equal
     (with-input-from-string
         (stream "#+(or (and test1 (not test2)) test3) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     'nil))))
(define-test sacla-must-reader.1334 (:tag :sacla)
 (assert-true
  (let ((*features* '(:test1 :test2 :test3)))
    (equal
     (with-input-from-string
         (stream "#+(or (and test1 (not test2)) test3) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     '(eat-this)))))
(define-test sacla-must-reader.1335 (:tag :sacla)
 (assert-true
  (let ((*features* '(:test1 :test3)))
    (equal
     (with-input-from-string
         (stream "#+(or (and test1 (not test2)) test3) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     '(eat-this)))))
(define-test sacla-must-reader.1336 (:tag :sacla)
 (assert-true
  (let ((*features* '(:test2 :test3)))
    (equal
     (with-input-from-string
         (stream "#+(or (and test1 (not test2)) test3) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     '(eat-this)))))
(define-test sacla-must-reader.1337 (:tag :sacla)
 (assert-true
  (let ((*features* 'nil))
    (equal
     (with-input-from-string
         (stream "#+(and test1 (not test2) (or test3 test4)) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     'nil))))
(define-test sacla-must-reader.1338 (:tag :sacla)
 (assert-true
  (let ((*features* '(:test1)))
    (equal
     (with-input-from-string
         (stream "#+(and test1 (not test2) (or test3 test4)) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     'nil))))
(define-test sacla-must-reader.1339 (:tag :sacla)
 (assert-true
  (let ((*features* '(:test1 :test3)))
    (equal
     (with-input-from-string
         (stream "#+(and test1 (not test2) (or test3 test4)) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     '(eat-this)))))
(define-test sacla-must-reader.1340 (:tag :sacla)
 (assert-true
  (let ((*features* '(:test1 :test4)))
    (equal
     (with-input-from-string
         (stream "#+(and test1 (not test2) (or test3 test4)) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     '(eat-this)))))
(define-test sacla-must-reader.1341 (:tag :sacla)
 (assert-true
  (let ((*features* '(:test1 :test2)))
    (equal
     (with-input-from-string
         (stream "#+(and test1 (not test2) (or test3 test4)) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     'nil))))
(define-test sacla-must-reader.1342 (:tag :sacla)
 (assert-true
  (let ((*features* '(:test1 :test2 :test3)))
    (equal
     (with-input-from-string
         (stream "#+(and test1 (not test2) (or test3 test4)) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     'nil))))
(define-test sacla-must-reader.1343 (:tag :sacla)
 (assert-true
  (let ((*features* '(:test1 :test2 :test3 :test4)))
    (equal
     (with-input-from-string
         (stream "#+(and test1 (not test2) (or test3 test4)) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     'nil))))
(define-test sacla-must-reader.1344 (:tag :sacla)
 (assert-true
  (let ((*features* '(:test1 :test3 :test4)))
    (equal
     (with-input-from-string
         (stream "#+(and test1 (not test2) (or test3 test4)) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     '(eat-this)))))
(define-test sacla-must-reader.1345 (:tag :sacla)
 (assert-true
  (let ((*features* 'nil))
    (equal
     (with-input-from-string
         (stream "#-(not (and test1 (not test2) (or test3 test4))) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     'nil))))
(define-test sacla-must-reader.1346 (:tag :sacla)
 (assert-true
  (let ((*features* '(:test1)))
    (equal
     (with-input-from-string
         (stream "#-(not (and test1 (not test2) (or test3 test4))) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     'nil))))
(define-test sacla-must-reader.1347 (:tag :sacla)
 (assert-true
  (let ((*features* '(:test1 :test3)))
    (equal
     (with-input-from-string
         (stream "#-(not (and test1 (not test2) (or test3 test4))) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     '(eat-this)))))
(define-test sacla-must-reader.1348 (:tag :sacla)
 (assert-true
  (let ((*features* '(:test1 :test4)))
    (equal
     (with-input-from-string
         (stream "#-(not (and test1 (not test2) (or test3 test4))) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     '(eat-this)))))
(define-test sacla-must-reader.1349 (:tag :sacla)
 (assert-true
  (let ((*features* '(:test1 :test2)))
    (equal
     (with-input-from-string
         (stream "#-(not (and test1 (not test2) (or test3 test4))) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     'nil))))
(define-test sacla-must-reader.1350 (:tag :sacla)
 (assert-true
  (let ((*features* '(:test1 :test2 :test3)))
    (equal
     (with-input-from-string
         (stream "#-(not (and test1 (not test2) (or test3 test4))) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     'nil))))
(define-test sacla-must-reader.1351 (:tag :sacla)
 (assert-true
  (let ((*features* '(:test1 :test2 :test3 :test4)))
    (equal
     (with-input-from-string
         (stream "#-(not (and test1 (not test2) (or test3 test4))) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     'nil))))
(define-test sacla-must-reader.1352 (:tag :sacla)
 (assert-true
  (let ((*features* '(:test1 :test3 :test4)))
    (equal
     (with-input-from-string
         (stream "#-(not (and test1 (not test2) (or test3 test4))) eat-this")
       (loop for x = (read stream nil 'end) until (eq x 'end) collecting x))
     '(eat-this)))))
(define-test sacla-must-reader.1353 (:tag :sacla)
 (assert-true (eq (read-from-string "#| comment |# a") 'a)))
(define-test sacla-must-reader.1354 (:tag :sacla)
 (assert-true (eq (read-from-string "#| #| nested comment |# |# a") 'a)))
(define-test sacla-must-reader.1355 (:tag :sacla)
 (assert-true
  (eq
   (read-from-string "#| comment
comment
   still comment
|# a")
   'a)))
(define-test sacla-must-reader.1356 (:tag :sacla)
 (assert-true
  (handler-case (read-from-string "#<invalid-token>")
    (reader-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-reader.1357 (:tag :sacla)
 (assert-true
  (handler-case (read-from-string "# ")
    (reader-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-reader.1358 (:tag :sacla)
 (assert-true
  (handler-case
      (read-from-string "#
")
    (reader-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-reader.1359 (:tag :sacla)
 (assert-true
  (handler-case (read-from-string "#)")
    (reader-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-reader.1360 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :upcase)
    (string= "ZEBRA" (symbol-name (read-from-string "ZEBRA"))))))
(define-test sacla-must-reader.1361 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :upcase)
    (string= "ZEBRA" (symbol-name (read-from-string "Zebra"))))))
(define-test sacla-must-reader.1362 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :upcase)
    (string= "ZEBRA" (symbol-name (read-from-string "zebra"))))))
(define-test sacla-must-reader.1363 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (string= "zebra" (symbol-name (read-from-string "ZEBRA"))))))
(define-test sacla-must-reader.1364 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (string= "zebra" (symbol-name (read-from-string "Zebra"))))))
(define-test sacla-must-reader.1365 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (string= "zebra" (symbol-name (read-from-string "zebra"))))))
(define-test sacla-must-reader.1366 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (string= "ZEBRA" (symbol-name (read-from-string "ZEBRA"))))))
(define-test sacla-must-reader.1367 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (string= "Zebra" (symbol-name (read-from-string "Zebra"))))))
(define-test sacla-must-reader.1368 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (string= "zebra" (symbol-name (read-from-string "zebra"))))))
(define-test sacla-must-reader.1369 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :invert)
    (string= "zebra" (symbol-name (read-from-string "ZEBRA"))))))
(define-test sacla-must-reader.1370 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :invert)
    (string= "Zebra" (symbol-name (read-from-string "Zebra"))))))
(define-test sacla-must-reader.1371 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :invert)
    (string= "ZEBRA" (symbol-name (read-from-string "zebra"))))))
(define-test sacla-must-reader.1372 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :upcase)
    (string= "CAT-AND-MOUSE"
             (symbol-name (read-from-string "cat-and-mouse"))))))
(define-test sacla-must-reader.1373 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :upcase)
    (string= "CAT-AND-MOUSE"
             (symbol-name (read-from-string "Cat-And-Mouse"))))))
(define-test sacla-must-reader.1374 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :upcase)
    (string= "CAT-AND-MOUSE"
             (symbol-name (read-from-string "CAT-AND-MOUSE"))))))
(define-test sacla-must-reader.1375 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (string= "cat-and-mouse"
             (symbol-name (read-from-string "cat-and-mouse"))))))
(define-test sacla-must-reader.1376 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (string= "cat-and-mouse"
             (symbol-name (read-from-string "Cat-And-Mouse"))))))
(define-test sacla-must-reader.1377 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (string= "cat-and-mouse"
             (symbol-name (read-from-string "CAT-AND-MOUSE"))))))
(define-test sacla-must-reader.1378 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (string= "cat-and-mouse"
             (symbol-name (read-from-string "cat-and-mouse"))))))
(define-test sacla-must-reader.1379 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (string= "Cat-And-Mouse"
             (symbol-name (read-from-string "Cat-And-Mouse"))))))
(define-test sacla-must-reader.1380 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (string= "CAT-AND-MOUSE"
             (symbol-name (read-from-string "CAT-AND-MOUSE"))))))
(define-test sacla-must-reader.1381 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :invert)
    (string= "CAT-AND-MOUSE"
             (symbol-name (read-from-string "cat-and-mouse"))))))
(define-test sacla-must-reader.1382 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :invert)
    (string= "Cat-And-Mouse"
             (symbol-name (read-from-string "Cat-And-Mouse"))))))
(define-test sacla-must-reader.1383 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :invert)
    (string= "cat-and-mouse"
             (symbol-name (read-from-string "CAT-AND-MOUSE"))))))
(define-test sacla-must-reader.1384 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :upcase)
    (string= "CAT*AND*MOUSE"
             (symbol-name (read-from-string "cat*and*mouse"))))))
(define-test sacla-must-reader.1385 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :upcase)
    (string= "CAT*AND*MOUSE"
             (symbol-name (read-from-string "Cat*And*Mouse"))))))
(define-test sacla-must-reader.1386 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :upcase)
    (string= "CAT*AND*MOUSE"
             (symbol-name (read-from-string "CAT*AND*MOUSE"))))))
(define-test sacla-must-reader.1387 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (string= "cat*and*mouse"
             (symbol-name (read-from-string "cat*and*mouse"))))))
(define-test sacla-must-reader.1388 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (string= "cat*and*mouse"
             (symbol-name (read-from-string "Cat*And*Mouse"))))))
(define-test sacla-must-reader.1389 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (string= "cat*and*mouse"
             (symbol-name (read-from-string "CAT*AND*MOUSE"))))))
(define-test sacla-must-reader.1390 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (string= "cat*and*mouse"
             (symbol-name (read-from-string "cat*and*mouse"))))))
(define-test sacla-must-reader.1391 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (string= "Cat*And*Mouse"
             (symbol-name (read-from-string "Cat*And*Mouse"))))))
(define-test sacla-must-reader.1392 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (string= "CAT*AND*MOUSE"
             (symbol-name (read-from-string "CAT*AND*MOUSE"))))))
(define-test sacla-must-reader.1393 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :invert)
    (string= "CAT*AND*MOUSE"
             (symbol-name (read-from-string "cat*and*mouse"))))))
(define-test sacla-must-reader.1394 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :invert)
    (string= "Cat*And*Mouse"
             (symbol-name (read-from-string "Cat*And*Mouse"))))))
(define-test sacla-must-reader.1395 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :invert)
    (string= "cat*and*mouse"
             (symbol-name (read-from-string "CAT*AND*MOUSE"))))))
(define-test sacla-must-reader.1396 (:tag :sacla)
 (assert-true
  (with-input-from-string (stream "a b")
    (and (eq 'a (read-preserving-whitespace stream))
         (eq #\  (read-char stream))
         (eq #\b (read-char stream))))))
(define-test sacla-must-reader.1397 (:tag :sacla)
 (assert-true
  (handler-case
      (with-input-from-string (stream " ")
        (read stream))
    (end-of-file nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-reader.1398 (:tag :sacla)
 (assert-true
  (let ((x nil))
    (and
     (eq t
         (handler-case
             (with-input-from-string (stream "a")
               (setq x (read stream))
               (read stream))
           (end-of-file nil t)
           (error nil nil)
           (:no-error (&rest rest) (declare (ignore rest)) nil)))
     (eq x 'a)))))
(define-test sacla-must-reader.1399 (:tag :sacla)
 (assert-true
  (progn
    (let ((*readtable* (copy-readtable nil)))
      (set-macro-character #\/
                           #'(lambda (stream char)
                               (declare (ignore char))
                               `(path
                                 ,@(loop for dir =
                                             (read-preserving-whitespace stream
                                                                         t)
                                             then
                                             (progn
                                               (read-char stream t nil t)
                                               (read-preserving-whitespace
                                                stream
                                                t))
                                         collect dir
                                         while (eql
                                                (peek-char nil
                                                           stream
                                                           nil
                                                           nil
                                                           t)
                                                #\/)))))
      (equal (read-from-string "(zyedh /usr/games/zork /usr/games/boggle)")
             '(zyedh (path usr games zork) (path usr games boggle)))))))
(define-test sacla-must-reader.1400 (:tag :sacla)
 (assert-true
  (progn
    (let ((*readtable* (copy-readtable nil)))
      (set-macro-character #\/
                           #'(lambda (stream char)
                               (declare (ignore char))
                               `(path
                                 ,@(loop for dir = (read stream t) then
                                             (progn
                                               (read-char stream t nil t)
                                               (read stream t))
                                         collect dir
                                         while (eql
                                                (peek-char nil
                                                           stream
                                                           nil
                                                           nil
                                                           t)
                                                #\/)))))
      (equal (read-from-string "(zyedh /usr/games/zork /usr/games/boggle)")
             '(zyedh (path usr games zork usr games boggle)))))))
(define-test sacla-must-reader.1401 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (and (eq t (set-syntax-from-char #\7 #\;))
         (= 1235 (read-from-string "123579"))))))
(define-test sacla-must-reader.1402 (:tag :sacla)
 (assert-true (readtablep *readtable*)))
(define-test sacla-must-reader.1403 (:tag :sacla)
 (assert-true (readtablep (copy-readtable))))
(define-test sacla-must-reader.1404 (:tag :sacla)
 (assert-true (readtablep (copy-readtable nil))))
(define-test sacla-must-reader.1405 (:tag :sacla)
 (assert-true (readtablep (copy-readtable nil (copy-readtable)))))
(define-test sacla-must-reader.1406 (:tag :sacla)
 (assert-true
  (let ((to (copy-readtable)))
    (eq to (copy-readtable nil to)))))
(define-test sacla-must-reader.1407 (:tag :sacla)
 (assert-true
  (let ((zvar 123) (table2 (copy-readtable)))
    (declare (special zvar))
    (and (= zvar 123)
         (set-syntax-from-char #\z #\' table2)
         (= zvar 123)
         (let ((*readtable* table2))
           (and (equal ''var (read-from-string "zvar"))
                (setq *readtable* (copy-readtable))
                (equal ''var (read-from-string "zvar"))
                (setq *readtable* (copy-readtable nil))
                (= 123 (eval (read-from-string "zvar")))))))))
(define-test sacla-must-reader.1408 (:tag :sacla)
 (assert-true (not (eq (copy-readtable) *readtable*))))
(define-test sacla-must-reader.1409 (:tag :sacla)
 (assert-true (not (eq (copy-readtable) (copy-readtable)))))
(define-test sacla-must-reader.1410 (:tag :sacla)
 (assert-true (not (eq (copy-readtable nil) *readtable*))))
(define-test sacla-must-reader.1411 (:tag :sacla)
 (assert-true (not (eq (copy-readtable nil) (copy-readtable nil)))))
(define-test sacla-must-reader.1412 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (and
     (handler-case (read-from-string "#<abc")
       (reader-error nil t)
       (error nil nil)
       (:no-error (&rest rest) (declare (ignore rest)) nil))
     (set-dispatch-macro-character #\#
                                   #\<
                                   #'(lambda (s c n)
                                       (declare (ignore c n))
                                       (read-char s t nil t)
                                       (read s t nil t)))
     (eq 'bc (read-from-string "#<abc"))
     (setq *readtable* (copy-readtable))
     (eq 'bc (read-from-string "#<abc"))
     (setq *readtable* (copy-readtable nil))
     (handler-case (read-from-string "#<abc")
       (reader-error nil t)
       (error nil nil)
       (:no-error (&rest rest) (declare (ignore rest)) nil))))))
(define-test sacla-must-reader.1413 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (and
     (handler-case (read-from-string "#<abc")
       (reader-error nil t)
       (error nil nil)
       (:no-error (&rest rest) (declare (ignore rest)) nil))
     (set-dispatch-macro-character #\#
                                   #\<
                                   #'(lambda (s c n)
                                       (declare (ignore c n))
                                       (read-char s t nil t)
                                       (read s t nil t)))
     (eq 'bc (read-from-string "#<abc"))
     (setq *readtable* (copy-readtable))
     (eq 'bc (read-from-string "#<abc"))
     (set-dispatch-macro-character #\#
                                   #\<
                                   #'(lambda (s c n)
                                       (declare (ignore c n))
                                       (read-char s t nil t)
                                       (read-char s t nil t)
                                       (read s t nil t)))
     (eq 'c (read-from-string "#<abc"))
     (setq *readtable* (copy-readtable nil))
     (handler-case (read-from-string "#<abc")
       (reader-error nil t)
       (error nil nil)
       (:no-error (&rest rest) (declare (ignore rest)) nil))))))
(define-test sacla-must-reader.1414 (:tag :sacla)
 (assert-true
  (let ((table (copy-readtable nil)))
    (and (eq :upcase (readtable-case table))
         (setf (readtable-case table) :invert)
         (let ((copy (copy-readtable table)))
           (and (not (eq table copy)) (eq (readtable-case copy) :invert)))))))
(define-test sacla-must-reader.1415 (:tag :sacla)
 (assert-true
  (let ((table (copy-readtable nil)) copy)
    (and (eq :upcase (readtable-case table))
         (setf (readtable-case table) :invert)
         (eq (readtable-case table) :invert)
         (setq copy (copy-readtable table))
         (eq (readtable-case copy) :invert)
         (setf (readtable-case copy) :preserve)
         (eq (readtable-case table) :invert)))))
(define-test sacla-must-reader.1416 (:tag :sacla)
 (assert-true
  (eq
   :upcase (let ((x (copy-readtable nil)))
             (readtable-case x)))))
(define-test sacla-must-reader.1417 (:tag :sacla)
 (assert-true
  (let ((x (copy-readtable nil)))
    (and (eq (setf (readtable-case x) :upcase) (readtable-case x))
         (eq (setf (readtable-case x) :downcase) (readtable-case x))
         (eq (setf (readtable-case x) :preserve) (readtable-case x))
         (eq (setf (readtable-case x) :invert) (readtable-case x))))))
(define-test sacla-must-reader.1418 (:tag :sacla)
 (assert-true
  (handler-case (readtable-case 'not-a-readtable)
    (type-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-reader.1419 (:tag :sacla)
 (assert-true
  (handler-case (setf (readtable-case (copy-readtable nil)) :no-such-mode)
    (type-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-reader.1420 (:tag :sacla)
 (assert-true
  (let ((table (copy-readtable nil)))
    (and (eq :upcase (readtable-case table))
         (setf (readtable-case table) :downcase)
         (eq :downcase (readtable-case (copy-readtable table)))))))
(define-test sacla-must-reader.1421 (:tag :sacla)
 (assert-true (not (readtablep nil))))
(define-test sacla-must-reader.1422 (:tag :sacla)
 (assert-true (not (readtablep 'readtable))))
(define-test sacla-must-reader.1423 (:tag :sacla)
 (assert-true (readtablep *readtable*)))
(define-test sacla-must-reader.1424 (:tag :sacla)
 (assert-true (readtablep (copy-readtable))))
(define-test sacla-must-reader.1425 (:tag :sacla)
 (assert-true (not (readtablep '*readtable*))))
(define-test sacla-must-reader.1426 (:tag :sacla)
 (assert-true (null (get-dispatch-macro-character #\# #\0))))
(define-test sacla-must-reader.1427 (:tag :sacla)
 (assert-true (null (get-dispatch-macro-character #\# #\1))))
(define-test sacla-must-reader.1428 (:tag :sacla)
 (assert-true (null (get-dispatch-macro-character #\# #\2))))
(define-test sacla-must-reader.1429 (:tag :sacla)
 (assert-true (null (get-dispatch-macro-character #\# #\3))))
(define-test sacla-must-reader.1430 (:tag :sacla)
 (assert-true (null (get-dispatch-macro-character #\# #\4))))
(define-test sacla-must-reader.1431 (:tag :sacla)
 (assert-true (null (get-dispatch-macro-character #\# #\5))))
(define-test sacla-must-reader.1432 (:tag :sacla)
 (assert-true (null (get-dispatch-macro-character #\# #\6))))
(define-test sacla-must-reader.1433 (:tag :sacla)
 (assert-true (null (get-dispatch-macro-character #\# #\7))))
(define-test sacla-must-reader.1434 (:tag :sacla)
 (assert-true (null (get-dispatch-macro-character #\# #\8))))
(define-test sacla-must-reader.1435 (:tag :sacla)
 (assert-true (null (get-dispatch-macro-character #\# #\9))))
(define-test sacla-must-reader.1436 (:tag :sacla)
 (assert-true (get-dispatch-macro-character #\# #\\)))
(define-test sacla-must-reader.1437 (:tag :sacla)
 (assert-true (get-dispatch-macro-character #\# #\')))
(define-test sacla-must-reader.1438 (:tag :sacla)
 (assert-true (get-dispatch-macro-character #\# #\()))
(define-test sacla-must-reader.1439 (:tag :sacla)
 (assert-true (get-dispatch-macro-character #\# #\*)))
(define-test sacla-must-reader.1440 (:tag :sacla)
 (assert-true (get-dispatch-macro-character #\# #\:)))
(define-test sacla-must-reader.1441 (:tag :sacla)
 (assert-true (get-dispatch-macro-character #\# #\.)))
(define-test sacla-must-reader.1442 (:tag :sacla)
 (assert-true (get-dispatch-macro-character #\# #\b)))
(define-test sacla-must-reader.1443 (:tag :sacla)
 (assert-true (get-dispatch-macro-character #\# #\o)))
(define-test sacla-must-reader.1444 (:tag :sacla)
 (assert-true (get-dispatch-macro-character #\# #\x)))
(define-test sacla-must-reader.1445 (:tag :sacla)
 (assert-true (get-dispatch-macro-character #\# #\r)))
(define-test sacla-must-reader.1446 (:tag :sacla)
 (assert-true (get-dispatch-macro-character #\# #\c)))
(define-test sacla-must-reader.1447 (:tag :sacla)
 (assert-true (get-dispatch-macro-character #\# #\a)))
(define-test sacla-must-reader.1448 (:tag :sacla)
 (assert-true (get-dispatch-macro-character #\# #\s)))
(define-test sacla-must-reader.1449 (:tag :sacla)
 (assert-true (get-dispatch-macro-character #\# #\p)))
(define-test sacla-must-reader.1450 (:tag :sacla)
 (assert-true (get-dispatch-macro-character #\# #\=)))
(define-test sacla-must-reader.1451 (:tag :sacla)
 (assert-true (get-dispatch-macro-character #\# #\#)))
(define-test sacla-must-reader.1452 (:tag :sacla)
 (assert-true (get-dispatch-macro-character #\# #\+)))
(define-test sacla-must-reader.1453 (:tag :sacla)
 (assert-true (get-dispatch-macro-character #\# #\-)))
(define-test sacla-must-reader.1454 (:tag :sacla)
 (assert-true (get-dispatch-macro-character #\# #\|)))
(define-test sacla-must-reader.1455 (:tag :sacla)
 (assert-true (get-dispatch-macro-character #\# #\Newline)))
(define-test sacla-must-reader.1456 (:tag :sacla)
 (assert-true (get-dispatch-macro-character #\# #\ )))
(define-test sacla-must-reader.1457 (:tag :sacla)
 (assert-true
  (handler-case (get-dispatch-macro-character #\a #\b)
    (error nil t)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-reader.1458 (:tag :sacla)
 (assert-true
  (handler-case (get-dispatch-macro-character #\a #\b nil)
    (error nil t)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-reader.1459 (:tag :sacla)
 (assert-true
  (handler-case (get-dispatch-macro-character #\a #\b *readtable*)
    (error nil t)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-reader.1460 (:tag :sacla)
 (assert-true
  (handler-case (set-dispatch-macro-character #\a #\b #'identity)
    (error nil t)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-reader.1461 (:tag :sacla)
 (assert-true
  (handler-case (set-dispatch-macro-character #\a #\b #'identity *readtable*)
    (error nil t)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-reader.1462 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (and
     (eq t
         (set-dispatch-macro-character #\#
                                       #\{
                                       #'(lambda (s c n)
                                           (declare (ignore c))
                                           (let ((list (read s nil (values) t)))
                                             (when (consp list)
                                               (unless
                                                   (and n (< 0 n (length list)))
                                                 (setq n 0))
                                               (setq list (nth n list)))
                                             list))))
     (= 1 (read-from-string "#{(1 2 3 4)"))
     (= 3 (read-from-string "#3{(0 1 2 3)"))
     (= 123 (read-from-string "#{123"))))))
(define-test sacla-must-reader.1463 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable))
        (dollar
         #'(lambda (stream subchar arg)
             (declare (ignore subchar arg))
             (list 'dollars (read stream t nil t)))))
    (and (eq t (set-dispatch-macro-character #\# #\$ dollar))
         (equal '(dollars foo) (read-from-string "#$foo"))))))
(define-test sacla-must-reader.1464 (:tag :sacla)
 (assert-true
  (and
   (let ((*readtable* (copy-readtable)))
     (and (setf (readtable-case *readtable*) :invert)
          (string= "ABC" (symbol-name (read-from-string "abc")))
          (string= "abc" (symbol-name (read-from-string "ABC")))
          (string= "AbC" (symbol-name (read-from-string "AbC")))
          (setf (readtable-case *readtable*) :preserve)
          (string= "abc" (symbol-name (read-from-string "abc")))
          (string= "ABC" (symbol-name (read-from-string "ABC")))
          (string= "AbC" (symbol-name (read-from-string "AbC")))))
   (eq (readtable-case *readtable*) :upcase)
   (string= "ABC" (symbol-name (read-from-string "abc")))
   (string= "ABC" (symbol-name (read-from-string "ABC")))
   (string= "ABC" (symbol-name (read-from-string "AbC"))))))
(define-test sacla-must-reader.1465 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable)))
    (and (setf (readtable-case *readtable*) :invert)
         (set-macro-character #\<
                              #'(lambda (stream c)
                                  (declare (ignore c))
                                  (read-delimited-list #\> stream t))
                              t)
         (set-macro-character #\> (get-macro-character #\)))
         (equal '(a b) (read-from-string "<a b>"))))))
(define-test sacla-must-reader.1466 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable)))
    (and (setf (readtable-case *readtable*) :invert)
         (set-macro-character #\<
                              #'(lambda (stream c)
                                  (declare (ignore c))
                                  (read-delimited-list #\> stream t)))
         (set-macro-character #\> (get-macro-character #\)))
         (with-input-from-string (stream "xyz<A b>jKl")
           (and (eq 'xyz (read stream))
                (equal '(|a| b) (read stream))
                (eq '|jKl| (read stream))
                (eq 'end (read stream nil 'end))))))))
(define-test sacla-must-reader.1467 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (and (equal (multiple-value-list (get-macro-character #\{)) '(nil nil))
         (eq t (make-dispatch-macro-character #\{))
         (get-macro-character #\{)))))
(define-test sacla-must-reader.1468 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (and (eq t (make-dispatch-macro-character #\{))
         (handler-case (read-from-string "{$a")
           (reader-error nil t)
           (error nil nil)
           (:no-error (&rest rest) (declare (ignore rest)) nil))))))
(define-test sacla-must-reader.1469 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (and (eq t (make-dispatch-macro-character #\{))
         (handler-case (read-from-string "{$a")
           (reader-error nil t)
           (error nil nil)
           (:no-error (&rest rest) (declare (ignore rest)) nil))
         (set-dispatch-macro-character #\{
                                       #\$
                                       #'(lambda (s c n)
                                           (declare (ignore c n))
                                           (read s t nil t)))
         (eq 'a (read-from-string "{$a"))))))
(define-test sacla-must-reader.1470 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (and (eq t (make-dispatch-macro-character #\{))
         (handler-case (read-from-string "{$a")
           (reader-error nil t)
           (error nil nil)
           (:no-error (&rest rest) (declare (ignore rest)) nil))
         (set-dispatch-macro-character #\{
                                       #\$
                                       #'(lambda (s c n)
                                           (declare (ignore c n))
                                           (read s t nil t)))
         (with-input-from-string (stream "xyz{$a")
           (and (eq 'xyz (read stream))
                (eq 'a (read stream))
                (eq 'end (read stream nil 'end))))))))
(define-test sacla-must-reader.1471 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (and (eq t (make-dispatch-macro-character #\{ t))
         (handler-case (read-from-string "{$a")
           (reader-error nil t)
           (error nil nil)
           (:no-error (&rest rest) (declare (ignore rest)) nil))
         (set-dispatch-macro-character #\{
                                       #\$
                                       #'(lambda (s c n)
                                           (declare (ignore c n))
                                           (read s t nil t)))
         (with-input-from-string (stream "xyz{$a")
           (and (eq 'xyz{$a (read stream))
                (eq 'end (read stream nil 'end))))))))
(define-test sacla-must-reader.1472 (:tag :sacla)
 (assert-true
  (let ((table (copy-readtable nil)))
    (and (eq t (make-dispatch-macro-character #\{ nil table))
         (let ((*readtable* table))
           (handler-case (read-from-string "{$a")
             (reader-error nil t)
             (error nil nil)
             (:no-error (&rest rest) (declare (ignore rest)) nil)))
         (set-dispatch-macro-character #\{
                                       #\$
                                       #'(lambda (s c n)
                                           (declare (ignore c n))
                                           (read s t nil t))
                                       table)
         (let ((*readtable* table))
           (with-input-from-string (stream "xyz{$a")
             (and (eq 'xyz (read stream))
                  (eq 'a (read stream))
                  (eq 'end (read stream nil 'end)))))))))
(define-test sacla-must-reader.1473 (:tag :sacla)
 (assert-true
  (let ((table (copy-readtable nil)))
    (and (eq t (make-dispatch-macro-character #\{ t table))
         (let ((*readtable* table))
           (handler-case (read-from-string "{$a")
             (reader-error nil t)
             (error nil nil)
             (:no-error (&rest rest) (declare (ignore rest)) nil)))
         (set-dispatch-macro-character #\{
                                       #\$
                                       #'(lambda (s c n)
                                           (declare (ignore c n))
                                           (read s t nil t))
                                       table)
         (let ((*readtable* table))
           (with-input-from-string (stream "xyz{$a")
             (and (eq 'xyz{$a (read stream))
                  (eq 'end (read stream nil 'end)))))))))
(define-test sacla-must-reader.1474 (:tag :sacla)
 (assert-true
  (with-input-from-string (stream "")
    (handler-case (read stream t)
      (end-of-file nil t)
      (error nil nil)
      (:no-error (&rest rest) (declare (ignore rest)) nil)))))
(define-test sacla-must-reader.1475 (:tag :sacla)
 (assert-true
  (with-input-from-string (stream "")
    (handler-case (read-preserving-whitespace stream t)
      (end-of-file nil t)
      (error nil nil)
      (:no-error (&rest rest) (declare (ignore rest)) nil)))))
(define-test sacla-must-reader.1476 (:tag :sacla)
 (assert-true
  (with-input-from-string (stream "")
    (handler-case (read stream t 'ignored)
      (end-of-file nil t)
      (error nil nil)
      (:no-error (&rest rest) (declare (ignore rest)) nil)))))
(define-test sacla-must-reader.1477 (:tag :sacla)
 (assert-true
  (with-input-from-string (stream "")
    (handler-case (read-preserving-whitespace stream t 'ignored)
      (end-of-file nil t)
      (error nil nil)
      (:no-error (&rest rest) (declare (ignore rest)) nil)))))
(define-test sacla-must-reader.1478 (:tag :sacla)
 (assert-true
  (with-input-from-string (stream "")
    (eq 'end (read stream nil 'end)))))
(define-test sacla-must-reader.1479 (:tag :sacla)
 (assert-true
  (with-input-from-string (stream "")
    (eq 'end (read-preserving-whitespace stream nil 'end)))))
(define-test sacla-must-reader.1480 (:tag :sacla)
 (assert-true
  (with-input-from-string (stream "a  b")
    (and (eq 'a (read-preserving-whitespace stream t nil nil))
         (equal (loop for c = (read-char stream nil nil) while c collecting c)
                '(#\  #\  #\b))))))
(define-test sacla-must-reader.1481 (:tag :sacla)
 (assert-true
  (with-input-from-string (stream "a  b")
    (and (eq 'a (read-preserving-whitespace stream t nil))
         (equal (loop for c = (read-char stream nil nil) while c collecting c)
                '(#\  #\  #\b))))))
(define-test sacla-must-reader.1482 (:tag :sacla)
 (assert-true
  (with-input-from-string (stream "ok")
    (let ((*standard-input* stream))
      (eq 'ok (read))))))
(define-test sacla-must-reader.1483 (:tag :sacla)
 (assert-true
  (with-input-from-string (stream "ok")
    (let ((*standard-input* stream))
      (eq 'ok (read-preserving-whitespace))))))
(define-test sacla-must-reader.1484 (:tag :sacla)
 (assert-true
  (with-input-from-string (stream "")
    (let ((*standard-input* stream))
      (handler-case (read)
        (end-of-file nil t)
        (error nil nil)
        (:no-error (&rest rest) (declare (ignore rest)) nil))))))
(define-test sacla-must-reader.1485 (:tag :sacla)
 (assert-true
  (with-input-from-string (stream "")
    (let ((*standard-input* stream))
      (handler-case (read-preserving-whitespace)
        (end-of-file nil t)
        (error nil nil)
        (:no-error (&rest rest) (declare (ignore rest)) nil))))))
(define-test sacla-must-reader.1486 (:tag :sacla)
 (assert-true
  (with-input-from-string (stream "")
    (let ((*standard-input* stream))
      (null (read nil nil))))))
(define-test sacla-must-reader.1487 (:tag :sacla)
 (assert-true
  (with-input-from-string (stream "")
    (let ((*standard-input* stream))
      (null (read-preserving-whitespace nil nil))))))
(define-test sacla-must-reader.1488 (:tag :sacla)
 (assert-true
  (with-input-from-string (*standard-input* "(a b")
    (handler-case (read)
      (end-of-file nil t)
      (error nil nil)
      (:no-error (&rest rest) (declare (ignore rest)) nil)))))
(define-test sacla-must-reader.1489 (:tag :sacla)
 (assert-true
  (with-input-from-string (*standard-input* "(a b")
    (handler-case (read-preserving-whitespace)
      (end-of-file nil t)
      (error nil nil)
      (:no-error (&rest rest) (declare (ignore rest)) nil)))))
(define-test sacla-must-reader.1490 (:tag :sacla)
 (assert-true
  (with-input-from-string (*standard-input* "(a (b")
    (handler-case (read)
      (end-of-file nil t)
      (error nil nil)
      (:no-error (&rest rest) (declare (ignore rest)) nil)))))
(define-test sacla-must-reader.1491 (:tag :sacla)
 (assert-true
  (with-input-from-string (*standard-input* "(a (b")
    (handler-case (read-preserving-whitespace)
      (end-of-file nil t)
      (error nil nil)
      (:no-error (&rest rest) (declare (ignore rest)) nil)))))
(define-test sacla-must-reader.1492 (:tag :sacla)
 (assert-true
  (with-input-from-string (*standard-input* "a b)")
    (equal '(a b) (read-delimited-list #\))))))
(define-test sacla-must-reader.1493 (:tag :sacla)
 (assert-true
  (with-input-from-string (*standard-input* ")")
    (null (read-delimited-list #\))))))
(define-test sacla-must-reader.1494 (:tag :sacla)
 (assert-true
  (with-input-from-string (*standard-input* "a b )")
    (equal '(a b) (read-delimited-list #\))))))
(define-test sacla-must-reader.1495 (:tag :sacla)
 (assert-true
  (with-input-from-string (*standard-input* "  a   b  )")
    (equal '(a b) (read-delimited-list #\))))))
(define-test sacla-must-reader.1496 (:tag :sacla)
 (assert-true
  (with-input-from-string (*standard-input* "  a   b    )   ")
    (equal '(a b) (read-delimited-list #\))))))
(define-test sacla-must-reader.1497 (:tag :sacla)
 (assert-true
  (with-input-from-string
      (*standard-input* "a b c d e f g h i j k l m n o p q r)")
    (equal '(a b c d e f g h i j k l m n o p q r) (read-delimited-list #\))))))
(define-test sacla-must-reader.1498 (:tag :sacla)
 (assert-true
  (with-input-from-string
      (*standard-input* "a (b) c (d) e f g h i j (k l m ) n o p q r)")
    (equal '(a (b) c (d) e f g h i j (k l m) n o p q r)
           (read-delimited-list #\))))))
(define-test sacla-must-reader.1499 (:tag :sacla)
 (assert-true
  (with-input-from-string (*standard-input* "a x\\)x b)")
    (equal '(a |X)X| b) (read-delimited-list #\))))))
(define-test sacla-must-reader.1500 (:tag :sacla)
 (assert-true
  (with-input-from-string (*standard-input* "a b) xyz")
    (and (equal '(a b) (read-delimited-list #\))) (eq 'xyz (read))))))
(define-test sacla-must-reader.1501 (:tag :sacla)
 (assert-true
  (with-input-from-string (*standard-input* "a #'car)")
    (equal '(a #'car) (read-delimited-list #\))))))
(define-test sacla-must-reader.1502 (:tag :sacla)
 (assert-true
  (with-input-from-string
      (*standard-input* "a #'car ;;
d #| e f |# g
z)")
    (equal '(a #'car d g z) (read-delimited-list #\))))))
(define-test sacla-must-reader.1503 (:tag :sacla)
 (assert-true
  (with-input-from-string
      (*standard-input* "a #'car ;;
d #| e f |# g
z)
xyz")
    (and (equal '(a #'car d g z) (read-delimited-list #\))) (eq 'xyz (read))))))
(define-test sacla-must-reader.1504 (:tag :sacla)
 (assert-true
  (with-input-from-string (*standard-input* "1 2 3 4 5 6 ]")
    (equal (read-delimited-list #\]) '(1 2 3 4 5 6)))))
(define-test sacla-must-reader.1505 (:tag :sacla)
 (assert-true (get-macro-character #\) nil)))
(define-test sacla-must-reader.1506 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil))
        (f
         #'(lambda (stream char arg)
             (declare (ignore char arg))
             (mapcon
              #'(lambda (x) (mapcar #'(lambda (y) (list (car x) y)) (cdr x)))
              (read-delimited-list #\} stream t)))))
    (set-dispatch-macro-character #\# #\{ f)
    (get-macro-character #\) nil)
    (set-macro-character #\} (get-macro-character #\) nil))
    (with-input-from-string (*standard-input* "#{ p q z a}")
      (equal (read) '((p q) (p z) (p a) (q z) (q a) (z a)))))))
(define-test sacla-must-reader.1507 (:tag :sacla)
 (assert-true
  (handler-case
      (with-input-from-string (stream "1 2 3 . 4)")
        (read-delimited-list #\) stream t))
    (error nil t)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-reader.1508 (:tag :sacla)
 (assert-true (get-dispatch-macro-character #\# #\( nil)))
(define-test sacla-must-reader.1509 (:tag :sacla)
 (assert-true (set-syntax-from-char #\z #\' (copy-readtable nil) nil)))
(define-test sacla-must-reader.1510 (:tag :sacla)
 (assert-true (equal '(abc 3) (multiple-value-list (read-from-string "abc")))))
(define-test sacla-must-reader.1511 (:tag :sacla)
 (assert-true
  (handler-case (read-from-string "")
    (end-of-file nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-reader.1512 (:tag :sacla)
 (assert-true
  (handler-case (read-from-string "" t 'ignored)
    (end-of-file nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-reader.1513 (:tag :sacla)
 (assert-true (eq 'end (read-from-string "" nil 'end))))
(define-test sacla-must-reader.1514 (:tag :sacla)
 (assert-true
  (equal '(b 5)
         (multiple-value-list
          (read-from-string "(a b c)" t nil :start 2 :end 6)))))
(define-test sacla-must-reader.1515 (:tag :sacla)
 (assert-true
  (equal '(b 4)
         (multiple-value-list
          (read-from-string "(a b  c)"
                            t
                            nil
                            :start 2
                            :preserve-whitespace t)))))
(define-test sacla-must-reader.1516 (:tag :sacla)
 (assert-true (null (read-from-string "" nil))))
(define-test sacla-must-reader.1517 (:tag :sacla)
 (assert-true
  (multiple-value-bind (thing pos)
      (read-from-string " a b" t nil :start 3)
    (and (eq thing 'b) (or (= pos 4) (= pos 5))))))
(define-test sacla-must-reader.1518 (:tag :sacla)
 (assert-true
  (multiple-value-bind (thing pos)
      (read-from-string "abcdefg" t nil :end 2)
    (and (eq thing 'ab) (or (= pos 2) (= pos 3))))))
(define-test sacla-must-reader.1519 (:tag :sacla)
 (assert-true
  (equal '(ijk 3)
         (multiple-value-list
          (read-from-string "ijk  xyz" t nil :preserve-whitespace t)))))
(define-test sacla-must-reader.1520 (:tag :sacla)
 (assert-true
  (equal '(def 7)
         (multiple-value-list
          (read-from-string "abc def ghi"
                            t
                            nil
                            :start 4
                            :end 9
                            :preserve-whitespace t)))))
(define-test sacla-must-reader.1521 (:tag :sacla)
 (assert-true (= 3 (read-from-string " 1 3 5" t nil :start 2))))
(define-test sacla-must-reader.1522 (:tag :sacla)
 (assert-true
  (multiple-value-bind (thing pos)
      (read-from-string "(a b c)")
    (and (equal thing '(a b c)) (or (= pos 7) (= pos 8))))))
(define-test sacla-must-reader.1523 (:tag :sacla)
 (assert-true
  (handler-case (read-from-string "(a b")
    (error nil t)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-reader.1524 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable)))
    (and
     (progn
       (handler-case (read-from-string "#<abc")
         (reader-error nil t)
         (error nil nil)
         (:no-error (&rest rest) (declare (ignore rest)) nil)))
     (set-dispatch-macro-character #\#
                                   #\<
                                   #'(lambda (s c n)
                                       (declare (ignore c n))
                                       (read-char s t nil t)
                                       (read s t nil t)))
     (eq 'bc (read-from-string "#<abc"))
     (setq *readtable* (copy-readtable))
     (eq 'bc (read-from-string "#<abc"))
     (set-dispatch-macro-character #\#
                                   #\<
                                   #'(lambda (s c n)
                                       (declare (ignore c n))
                                       (read-char s t nil t)
                                       (read-char s t nil t)
                                       (read s t nil t)))
     (eq 'c (read-from-string "#<abc"))
     (setq *readtable* (copy-readtable nil))
     (progn
       (handler-case (read-from-string "#<abc")
         (reader-error nil t)
         (error nil nil)
         (:no-error (&rest rest) (declare (ignore rest)) nil)))))))
(define-test sacla-must-reader.1525 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable)))
    (and (eq t (make-dispatch-macro-character #\{))
         (eq t
             (set-dispatch-macro-character #\{
                                           #\s
                                           #'(lambda (s c n)
                                               (declare (ignore c n))
                                               `(section ,(read s t nil t)))))
         (equal '(section (x y z)) (read-from-string "{s (x y z)"))
         (equal '(section (x y z)) (read-from-string "{S (x y z)"))))))
(define-test sacla-must-reader.1526 (:tag :sacla)
 (assert-true
  (multiple-value-bind #'non-terminating-p
      (get-macro-character #\")
    (and function (not non-terminating-p)))))
(define-test sacla-must-reader.1527 (:tag :sacla)
 (assert-true
  (multiple-value-bind #'non-terminating-p
      (get-macro-character #\#)
    (and function non-terminating-p))))
(define-test sacla-must-reader.1528 (:tag :sacla)
 (assert-true
  (multiple-value-bind #'non-terminating-p
      (get-macro-character #\')
    (and function (not non-terminating-p)))))
(define-test sacla-must-reader.1529 (:tag :sacla)
 (assert-true
  (multiple-value-bind #'non-terminating-p
      (get-macro-character #\()
    (and function (not non-terminating-p)))))
(define-test sacla-must-reader.1530 (:tag :sacla)
 (assert-true
  (multiple-value-bind #'non-terminating-p
      (get-macro-character #\))
    (and function (not non-terminating-p)))))
(define-test sacla-must-reader.1531 (:tag :sacla)
 (assert-true
  (multiple-value-bind #'non-terminating-p
      (get-macro-character #\,)
    (and function (not non-terminating-p)))))
(define-test sacla-must-reader.1532 (:tag :sacla)
 (assert-true
  (multiple-value-bind #'non-terminating-p
      (get-macro-character #\;)
    (and function (not non-terminating-p)))))
(define-test sacla-must-reader.1533 (:tag :sacla)
 (assert-true
  (multiple-value-bind #'non-terminating-p
      (get-macro-character #\`)
    (and function (not non-terminating-p)))))
(define-test sacla-must-reader.1534 (:tag :sacla)
 (assert-true
  (multiple-value-bind #'non-terminating-p
      (get-macro-character #\a)
    (and (null function) (not non-terminating-p)))))
(define-test sacla-must-reader.1535 (:tag :sacla)
 (assert-true
  (multiple-value-bind #'non-terminating-p
      (get-macro-character #\z)
    (and (null function) (not non-terminating-p)))))
(define-test sacla-must-reader.1536 (:tag :sacla)
 (assert-true
  (multiple-value-bind #'non-terminating-p
      (get-macro-character #\ )
    (and (null function) (not non-terminating-p)))))
(define-test sacla-must-reader.1537 (:tag :sacla)
 (assert-true
  (multiple-value-bind #'non-terminating-p
      (get-macro-character #\Tab)
    (and (null function) (not non-terminating-p)))))
(define-test sacla-must-reader.1538 (:tag :sacla)
 (assert-true
  (multiple-value-bind #'non-terminating-p
      (get-macro-character #\" nil)
    (and function (not non-terminating-p)))))
(define-test sacla-must-reader.1539 (:tag :sacla)
 (assert-true
  (multiple-value-bind #'non-terminating-p
      (get-macro-character #\# nil)
    (and function non-terminating-p))))
(define-test sacla-must-reader.1540 (:tag :sacla)
 (assert-true
  (multiple-value-bind #'non-terminating-p
      (get-macro-character #\' nil)
    (and function (not non-terminating-p)))))
(define-test sacla-must-reader.1541 (:tag :sacla)
 (assert-true
  (multiple-value-bind #'non-terminating-p
      (get-macro-character #\( nil)
    (and function (not non-terminating-p)))))
(define-test sacla-must-reader.1542 (:tag :sacla)
 (assert-true
  (multiple-value-bind #'non-terminating-p
      (get-macro-character #\) nil)
    (and function (not non-terminating-p)))))
(define-test sacla-must-reader.1543 (:tag :sacla)
 (assert-true
  (multiple-value-bind #'non-terminating-p
      (get-macro-character #\, nil)
    (and function (not non-terminating-p)))))
(define-test sacla-must-reader.1544 (:tag :sacla)
 (assert-true
  (multiple-value-bind #'non-terminating-p
      (get-macro-character #\; nil)
    (and function (not non-terminating-p)))))
(define-test sacla-must-reader.1545 (:tag :sacla)
 (assert-true
  (multiple-value-bind #'non-terminating-p
      (get-macro-character #\` nil)
    (and function (not non-terminating-p)))))
(define-test sacla-must-reader.1546 (:tag :sacla)
 (assert-true
  (multiple-value-bind #'non-terminating-p
      (get-macro-character #\a nil)
    (and (null function) (not non-terminating-p)))))
(define-test sacla-must-reader.1547 (:tag :sacla)
 (assert-true
  (multiple-value-bind #'non-terminating-p
      (get-macro-character #\z nil)
    (and (null function) (not non-terminating-p)))))
(define-test sacla-must-reader.1548 (:tag :sacla)
 (assert-true
  (multiple-value-bind #'non-terminating-p
      (get-macro-character #\  nil)
    (and (null function) (not non-terminating-p)))))
(define-test sacla-must-reader.1549 (:tag :sacla)
 (assert-true
  (multiple-value-bind #'non-terminating-p
      (get-macro-character #\Tab nil)
    (and (null function) (not non-terminating-p)))))
(define-test sacla-must-reader.1550 (:tag :sacla)
 (assert-true
  (and
   (let ((*readtable* (copy-readtable)))
     (and
      (eq t
          (set-macro-character #\$
                               #'(lambda (s c)
                                   (declare (ignore c))
                                   `(dollars ,(read s t nil t)))))
      (equal '(dollars 100) (read-from-string "$100"))
      (eq '$100 (read-from-string "\\$100"))
      (eq '$100 (read-from-string "|$|100"))))
   (null (get-macro-character #\$))
   (eq '$100 (read-from-string "$100")))))
(define-test sacla-must-reader.1551 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable)))
    (and (eq t (set-syntax-from-char #\[ #\())
         (equal '(0 1 2 3) (read-from-string "[0 1 2 3)"))))))
(define-test sacla-must-reader.1552 (:tag :sacla)
 (assert-true
  (let ((table1 (copy-readtable nil)) (table2 (copy-readtable nil)))
    (and (eq t (set-syntax-from-char #\[ #\( table1 table1))
         (equal '(0 1 2 3)
                (let ((*readtable* table1))
                  (read-from-string "[0 1 2 3)")))
         (eq t (set-syntax-from-char #\{ #\[ table2 table1))
         (equal '(0 1 2 3)
                (let ((*readtable* table2))
                  (read-from-string "{0 1 2 3)")))))))
(define-test sacla-must-reader.1553 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable)))
    (and (eq t (set-syntax-from-char #\[ #\.))
         (eq '3[0 (read-from-string "3[0"))))))
(define-test sacla-must-reader.1554 (:tag :sacla)
 (assert-true
  (let* ((str
          (concatenate 'string
                       (loop repeat 100 collecting #\()
                       "kernel"
                       (loop repeat 100 collecting #\))))
         (thing (read-from-string str)))
    (and (= 1 (length thing))
         (eq 'kernel
             (loop repeat 101
                   for x = thing then (car x)
                   finally (return x)))))))
(define-test sacla-must-reader.1555 (:tag :sacla)
 (assert-true
  (null
   (let ((*read-suppress* t))
     (read-from-string "abc")))))
(define-test sacla-must-reader.1556 (:tag :sacla)
 (assert-true
  (null
   (let ((*read-suppress* t))
     (with-input-from-string (stream "abc")
       (read stream))))))
(define-test sacla-must-reader.1557 (:tag :sacla)
 (assert-true
  (null
   (let ((*read-suppress* t))
     (with-input-from-string (stream "abc")
       (read-preserving-whitespace stream))))))
(define-test sacla-must-reader.1558 (:tag :sacla)
 (assert-true
  (null
   (let ((*read-suppress* t))
     (with-input-from-string (stream "abc xyz)")
       (read-delimited-list #\) stream))))))
(define-test sacla-must-reader.1559 (:tag :sacla)
 (assert-true
  (flet ((num2str (n base)
           (let* ((base-digits "0123456789ABCDEFGHIJKLMNOPQRSTUV")
                  (minus-p (< n 0))
                  (n (if minus-p (- n) n))
                  digits)
             (loop with x = n
                   do (multiple-value-bind (q r)
                          (floor x base)
                        (push (aref base-digits r) digits)
                        (setq x q)
                        (when (zerop q)
                          (return))))
             (when minus-p
               (push #\- digits))
             (make-array (length digits)
                         :element-type 'character
                         :initial-contents digits))))
    (loop for base from 2 upto 32
          always (loop for n from -100 upto 100
                       always (= n
                                 (let ((*read-base* base))
                                   (read-from-string (num2str n base)))))))))
(define-test sacla-must-reader.1560 (:tag :sacla)
 (assert-true
  (labels ((int2str (n base)
             (let* ((base-digits "0123456789ABCDEFGHIJKLMNOPQRSTUV")
                    (minus-p (< n 0))
                    (n (if minus-p (- n) n))
                    digits)
               (loop with x = n
                     do (multiple-value-bind (q r)
                            (floor x base)
                          (push (aref base-digits r) digits)
                          (setq x q)
                          (when (zerop q)
                            (return))))
               (when minus-p
                 (push #\- digits))
               (make-array (length digits)
                           :element-type 'character
                           :initial-contents digits)))
           (ratio2str (r base)
             (concatenate 'string
                          (int2str (numerator r) base)
                          "/"
                          (int2str (denominator r) base))))
    (loop for base from 2 upto 32
          always (loop for numerator from -100 upto 100 by 23
                       always (loop for denominator from 1 upto 300 by 51
                                    always (= (/ numerator denominator)
                                              (let ((*read-base* base))
                                                (read-from-string
                                                 (ratio2str
                                                  (/ numerator denominator)
                                                  base))))))))))

