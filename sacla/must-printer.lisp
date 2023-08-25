(in-package #:sacla-lisp-unit)
(define-test sacla-must-printer.1 (:tag :sacla)
 (assert-true (eql *print-base* 10)))
(define-test sacla-must-printer.2 (:tag :sacla)
 (assert-true (null *print-radix*)))
(define-test sacla-must-printer.3 (:tag :sacla)
 (assert-true (eq *print-case* :upcase)))
(define-test sacla-must-printer.4 (:tag :sacla) (assert-true *print-gensym*))
(define-test sacla-must-printer.5 (:tag :sacla)
 (assert-true (null *print-level*)))
(define-test sacla-must-printer.6 (:tag :sacla)
 (assert-true (null *print-length*)))
(define-test sacla-must-printer.7 (:tag :sacla)
 (assert-true (null *print-circle*)))
(define-test sacla-must-printer.8 (:tag :sacla) (assert-true *print-escape*))
(define-test sacla-must-printer.9 (:tag :sacla)
 (assert-true (null *print-readably*)))
(define-test sacla-must-printer.10 (:tag :sacla)
 (assert-true *print-pprint-dispatch*))
(define-test sacla-must-printer.11 (:tag :sacla)
 (assert-true (null *print-lines*)))
(define-test sacla-must-printer.12 (:tag :sacla)
 (assert-true (null *print-right-margin*)))
(define-test sacla-must-printer.13 (:tag :sacla)
 (assert-true (string= "abc" (write-to-string "abc" :escape nil))))
(define-test sacla-must-printer.14 (:tag :sacla)
 (assert-true (string= "\"abc\"" (write-to-string "abc" :readably t))))
(define-test sacla-must-printer.15 (:tag :sacla)
 (assert-true
  (string= "\"abc\"" (write-to-string "abc" :escape nil :readably t))))
(define-test sacla-must-printer.16 (:tag :sacla)
 (assert-true (string= "ABC" (write-to-string "ABC" :escape nil))))
(define-test sacla-must-printer.17 (:tag :sacla)
 (assert-true (string= "\"ABC\"" (write-to-string "ABC" :readably t))))
(define-test sacla-must-printer.18 (:tag :sacla)
 (assert-true
  (string= "\"ABC\"" (write-to-string "ABC" :escape nil :readably t))))
(define-test sacla-must-printer.19 (:tag :sacla)
 (assert-true
  (string= "\"A\\\\B\\\"C\""
           (write-to-string "A\\B\"C" :escape nil :readably t))))
(define-test sacla-must-printer.20 (:tag :sacla)
 (assert-true (string= "\"A\\\\B\\\"C\"" (write-to-string "A\\B\"C"))))
(define-test sacla-must-printer.21 (:tag :sacla)
 (assert-true (string= "A\\B\"C" (write-to-string "A\\B\"C" :escape nil))))
(define-test sacla-must-printer.22 (:tag :sacla)
 (assert-true
  (let ((str "a\\b\""))
    (and (= 4 (length str))
         (string= str (read-from-string (write-to-string str)))))))
(define-test sacla-must-printer.23 (:tag :sacla)
 (assert-true
  (let ((str "a\\b\""))
    (and (= 4 (length str))
         (string= str
                  (read-from-string
                   (write-to-string str :escape nil :readably t)))))))
(define-test sacla-must-printer.24 (:tag :sacla)
 (assert-true (string= "\"\\\"\"" (write-to-string "\""))))
(define-test sacla-must-printer.25 (:tag :sacla)
 (assert-true
  (string= "\"\\\"\"" (write-to-string "\"" :escape nil :readably t))))
(define-test sacla-must-printer.26 (:tag :sacla)
 (assert-true (string= "\"" (read-from-string (write-to-string "\"")))))
(define-test sacla-must-printer.27 (:tag :sacla)
 (assert-true
  (string= "\""
           (read-from-string (write-to-string "\"" :escape nil :readably t)))))
(define-test sacla-must-printer.28 (:tag :sacla)
 (assert-true (string= "\"\"" (write-to-string ""))))
(define-test sacla-must-printer.29 (:tag :sacla)
 (assert-true (string= "\"\"" (write-to-string "" :escape nil :readably t))))
(define-test sacla-must-printer.30 (:tag :sacla)
 (assert-true (string= "" (write-to-string "" :escape nil))))
(define-test sacla-must-printer.31 (:tag :sacla)
 (assert-true (string= "\" \"" (write-to-string " "))))
(define-test sacla-must-printer.32 (:tag :sacla)
 (assert-true (string= "\" \"" (write-to-string " " :escape nil :readably t))))
(define-test sacla-must-printer.33 (:tag :sacla)
 (assert-true (string= " " (write-to-string " " :escape nil))))
(define-test sacla-must-printer.34 (:tag :sacla)
 (assert-true (string= "\"	\"" (write-to-string "	"))))
(define-test sacla-must-printer.35 (:tag :sacla)
 (assert-true (string= "\"	\"" (write-to-string "	" :escape nil :readably t))))
(define-test sacla-must-printer.36 (:tag :sacla)
 (assert-true (string= "	" (write-to-string "	" :escape nil))))
(define-test sacla-must-printer.37 (:tag :sacla)
 (assert-true
  (string= "\"
\""
           (write-to-string "
"
                            :escape nil
                            :readably t))))
(define-test sacla-must-printer.38 (:tag :sacla)
 (assert-true
  (string= "
"
           (write-to-string "
"
                            :escape nil))))
(define-test sacla-must-printer.39 (:tag :sacla)
 (assert-true
  (string= "\"\\\"\\\"\\\"\\\"\\\"\\\"\""
           (write-to-string "\"\"\"\"\"\"" :readably t))))
(define-test sacla-must-printer.40 (:tag :sacla)
 (assert-true
  (string= "\"\"\"\"\"\""
           (read-from-string (write-to-string "\"\"\"\"\"\"" :readably t)))))
(define-test sacla-must-printer.41 (:tag :sacla)
 (assert-true
  (string= "\"\"\"\"\"\""
           (write-to-string "\"\"\"\"\"\"" :readably nil :escape nil))))
(define-test sacla-must-printer.42 (:tag :sacla)
 (assert-true (string= "\"	 	\"" (write-to-string "	 	" :readably t))))
(define-test sacla-must-printer.43 (:tag :sacla)
 (assert-true
  (string= "\"\\\"Hi\\\" \\\"Oh, hi!\\\"\""
           (write-to-string "\"Hi\" \"Oh, hi!\"" :readably t))))
(define-test sacla-must-printer.44 (:tag :sacla)
 (assert-true
  (string= "\"Hi\" \"Oh, hi!\""
           (write-to-string "\"Hi\" \"Oh, hi!\""
                            :pretty nil
                            :readably nil
                            :escape nil))))
(define-test sacla-must-printer.45 (:tag :sacla)
 (assert-true (string= "abc" (write-to-string "abc" :array nil :escape nil))))
(define-test sacla-must-printer.46 (:tag :sacla)
 (assert-true
  (string= "abc"
           (write-to-string
            (make-array 10
                        :element-type 'character
                        :initial-contents "abcdefghij"
                        :fill-pointer 3)
            :escape nil))))
(define-test sacla-must-printer.47 (:tag :sacla)
 (assert-true (string= (write-to-string 0) "0")))
(define-test sacla-must-printer.48 (:tag :sacla)
 (assert-true (string= (write-to-string 0) "0")))
(define-test sacla-must-printer.49 (:tag :sacla)
 (assert-true (string= (write-to-string 9) "9")))
(define-test sacla-must-printer.50 (:tag :sacla)
 (assert-true (string= (write-to-string -10) "-10")))
(define-test sacla-must-printer.51 (:tag :sacla)
 (assert-true
  (string= (write-to-string 1234567890987654321234567890987654321)
           "1234567890987654321234567890987654321")))
(define-test sacla-must-printer.52 (:tag :sacla)
 (assert-true
  (let ((*print-radix* t))
    (string= (write-to-string 0) "0."))))
(define-test sacla-must-printer.53 (:tag :sacla)
 (assert-true
  (let ((*print-radix* t))
    (string= (write-to-string -52) "-52."))))
(define-test sacla-must-printer.54 (:tag :sacla)
 (assert-true
  (let ((*print-radix* t))
    (string= (write-to-string -1234567890987654321234567890987654321)
             "-1234567890987654321234567890987654321."))))
(define-test sacla-must-printer.55 (:tag :sacla)
 (assert-true
  (let ((*print-base* 2))
    (string= (write-to-string 0) "0"))))
(define-test sacla-must-printer.56 (:tag :sacla)
 (assert-true
  (let ((*print-base* 2))
    (string= (write-to-string 10) "1010"))))
(define-test sacla-must-printer.57 (:tag :sacla)
 (assert-true
  (let ((*print-base* 2))
    (string= (write-to-string -1234567890987654321234567890987654321)
             "-111011011100010011100101100000010011000101110111101001110100010101110010000101001111011010110110001011000001110010110001"))))
(define-test sacla-must-printer.58 (:tag :sacla)
 (assert-true
  (let ((*print-base* 2) (*print-radix* t))
    (string= (write-to-string 11) "#b1011"))))
(define-test sacla-must-printer.59 (:tag :sacla)
 (assert-true
  (let ((*print-base* 2) (*print-radix* t))
    (string= (write-to-string -15) "#b-1111"))))
(define-test sacla-must-printer.60 (:tag :sacla)
 (assert-true
  (let ((*print-base* 2) (*print-radix* t))
    (string= (write-to-string 1234567890987654321234567890987654321)
             "#b111011011100010011100101100000010011000101110111101001110100010101110010000101001111011010110110001011000001110010110001"))))
(define-test sacla-must-printer.61 (:tag :sacla)
 (assert-true
  (let ((*print-base* 8))
    (string= (write-to-string 10) "12"))))
(define-test sacla-must-printer.62 (:tag :sacla)
 (assert-true
  (let ((*print-base* 8))
    (string= (write-to-string -21) "-25"))))
(define-test sacla-must-printer.63 (:tag :sacla)
 (assert-true
  (let ((*print-base* 8) (*print-radix* t))
    (string= (write-to-string 11) "#o13"))))
(define-test sacla-must-printer.64 (:tag :sacla)
 (assert-true
  (let ((*print-base* 8) (*print-radix* t))
    (string= (write-to-string -13) "#o-15"))))
(define-test sacla-must-printer.65 (:tag :sacla)
 (assert-true
  (let ((*print-base* 8))
    (string= (write-to-string 1234567890987654321234567890987654321)
             "7334234540230567516425620517326613016261"))))
(define-test sacla-must-printer.66 (:tag :sacla)
 (assert-true
  (let ((*print-base* 8) (*print-radix* t))
    (string= (write-to-string -1234567890987654321234567890987654321)
             "#o-7334234540230567516425620517326613016261"))))
(define-test sacla-must-printer.67 (:tag :sacla)
 (assert-true
  (let ((*print-base* 16))
    (string= (write-to-string 20) "14"))))
(define-test sacla-must-printer.68 (:tag :sacla)
 (assert-true
  (let ((*print-base* 16))
    (string= (write-to-string -22) "-16"))))
(define-test sacla-must-printer.69 (:tag :sacla)
 (assert-true
  (let ((*print-base* 16))
    (string= (string-upcase (write-to-string -30)) "-1E"))))
(define-test sacla-must-printer.70 (:tag :sacla)
 (assert-true
  (let ((*print-base* 16) (*print-radix* t))
    (string= (write-to-string 21) "#x15"))))
(define-test sacla-must-printer.71 (:tag :sacla)
 (assert-true
  (let ((*print-base* 16) (*print-radix* t))
    (string= (write-to-string -23) "#x-17"))))
(define-test sacla-must-printer.72 (:tag :sacla)
 (assert-true
  (let ((*print-base* 16))
    (string=
     (string-upcase (write-to-string 1234567890987654321234567890987654321))
     "EDC4E5813177A7457214F6B62C1CB1"))))
(define-test sacla-must-printer.73 (:tag :sacla)
 (assert-true
  (let ((*print-base* 16) (*print-radix* t))
    (string=
     (string-upcase (write-to-string -1234567890987654321234567890987654321))
     "#X-EDC4E5813177A7457214F6B62C1CB1"))))
(define-test sacla-must-printer.74 (:tag :sacla)
 (assert-true
  (let ((*print-base* 24))
    (string= (write-to-string 9) "9"))))
(define-test sacla-must-printer.75 (:tag :sacla)
 (assert-true
  (let ((*print-base* 24))
    (string= (string-upcase (write-to-string 17)) "H"))))
(define-test sacla-must-printer.76 (:tag :sacla)
 (assert-true
  (let ((*print-base* 24))
    (string= (string-upcase (write-to-string -17)) "-H"))))
(define-test sacla-must-printer.77 (:tag :sacla)
 (assert-true
  (let ((*print-base* 24) (*print-radix* t))
    (string= (write-to-string 9) "#24r9"))))
(define-test sacla-must-printer.78 (:tag :sacla)
 (assert-true
  (let ((*print-base* 24) (*print-radix* t))
    (string-equal (write-to-string 23) "#24rN"))))
(define-test sacla-must-printer.79 (:tag :sacla)
 (assert-true
  (let ((*print-base* 24) (*print-radix* t))
    (string-equal (write-to-string -23) "#24r-N"))))
(define-test sacla-must-printer.80 (:tag :sacla)
 (assert-true
  (let ((*print-base* 24))
    (string=
     (string-upcase (write-to-string 1234567890987654321234567890987654321))
     "1EDFC9EAF544D8D12FI44J4FMCH"))))
(define-test sacla-must-printer.81 (:tag :sacla)
 (assert-true
  (loop for *print-base* from 2 upto 36
        always (string= (write-to-string 0) "0"))))
(define-test sacla-must-printer.82 (:tag :sacla)
 (assert-true
  (loop for *print-base* from 2 upto 36
        always (string= (write-to-string -1) "-1"))))
(define-test sacla-must-printer.83 (:tag :sacla)
 (assert-true
  (loop for *print-base* from 2 upto 36
        always (string= (string-upcase (write-to-string (1- *print-base*)))
                        (string
                         (char "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                               (1- *print-base*)))))))
(define-test sacla-must-printer.84 (:tag :sacla)
 (assert-true
  (loop for *print-base* from 2 upto 36
        always (string= (write-to-string *print-base*) "10"))))
(define-test sacla-must-printer.85 (:tag :sacla)
 (assert-true
  (let ((list nil))
    (equal
     (dotimes (i 35 (reverse list))
       (let ((*print-base* (+ i 2)))
         (push (string-upcase (write-to-string 40)) list)))
     '("101000" "1111" "220" "130" "104" "55" "50" "44" "40" "37" "34" "31"
       "2C" "2A" "28" "26" "24" "22" "20" "1J" "1I" "1H" "1G" "1F" "1E" "1D"
       "1C" "1B" "1A" "19" "18" "17" "16" "15" "14")))))
(define-test sacla-must-printer.86 (:tag :sacla)
 (assert-true
  (let ((list nil))
    (equal
     (dotimes (i 35 (reverse list))
       (let ((*print-base* (+ i 2)) (*print-radix* t))
         (push (string-upcase (write-to-string 40)) list)))
     '("#B101000" "#3R1111" "#4R220" "#5R130" "#6R104" "#7R55" "#O50" "#9R44"
       "40." "#11R37" "#12R34" "#13R31" "#14R2C" "#15R2A" "#X28" "#17R26"
       "#18R24" "#19R22" "#20R20" "#21R1J" "#22R1I" "#23R1H" "#24R1G" "#25R1F"
       "#26R1E" "#27R1D" "#28R1C" "#29R1B" "#30R1A" "#31R19" "#32R18" "#33R17"
       "#34R16" "#35R15" "#36R14")))))
(define-test sacla-must-printer.87 (:tag :sacla)
 (assert-true (string= (write-to-string 1/3) "1/3")))
(define-test sacla-must-printer.88 (:tag :sacla)
 (assert-true (string= (write-to-string -1/2) "-1/2")))
(define-test sacla-must-printer.89 (:tag :sacla)
 (assert-true (string= (write-to-string -3/5) "-3/5")))
(define-test sacla-must-printer.90 (:tag :sacla)
 (assert-true
  (let ((*print-radix* t))
    (string= (write-to-string 1/15) "#10r1/15"))))
(define-test sacla-must-printer.91 (:tag :sacla)
 (assert-true
  (let ((*print-radix* t))
    (string= (write-to-string -4/15) "#10r-4/15"))))
(define-test sacla-must-printer.92 (:tag :sacla)
 (assert-true
  (string= (write-to-string 2/1234567890987654321234567890987654321)
           "2/1234567890987654321234567890987654321")))
(define-test sacla-must-printer.93 (:tag :sacla)
 (assert-true
  (string= (write-to-string 1234567890987654321234567890987654321/4)
           "1234567890987654321234567890987654321/4")))
(define-test sacla-must-printer.94 (:tag :sacla)
 (assert-true
  (let ((*print-radix* t))
    (string= (write-to-string 2/1234567890987654321234567890987654321)
             "#10r2/1234567890987654321234567890987654321"))))
(define-test sacla-must-printer.95 (:tag :sacla)
 (assert-true
  (let ((*print-base* 2))
    (string= (write-to-string 1/3) "1/11"))))
(define-test sacla-must-printer.96 (:tag :sacla)
 (assert-true
  (let ((*print-base* 2))
    (string= (write-to-string -1/2) "-1/10"))))
(define-test sacla-must-printer.97 (:tag :sacla)
 (assert-true
  (let ((*print-base* 2))
    (string= (write-to-string -3/5) "-11/101"))))
(define-test sacla-must-printer.98 (:tag :sacla)
 (assert-true
  (let ((*print-base* 2) (*print-radix* t))
    (string= (write-to-string 1/15) "#b1/1111"))))
(define-test sacla-must-printer.99 (:tag :sacla)
 (assert-true
  (let ((*print-base* 2) (*print-radix* t))
    (string= (write-to-string -3/16) "#b-11/10000"))))
(define-test sacla-must-printer.100 (:tag :sacla)
 (assert-true
  (let ((*print-base* 2))
    (string= (write-to-string 2/1234567890987654321234567890987654321)
             "10/111011011100010011100101100000010011000101110111101001110100010101110010000101001111011010110110001011000001110010110001"))))
(define-test sacla-must-printer.101 (:tag :sacla)
 (assert-true
  (let ((*print-base* 2))
    (string= (write-to-string -1234567890987654321234567890987654321/2)
             "-111011011100010011100101100000010011000101110111101001110100010101110010000101001111011010110110001011000001110010110001/10"))))
(define-test sacla-must-printer.102 (:tag :sacla)
 (assert-true
  (let ((*print-base* 2) (*print-radix* t))
    (string= (write-to-string 2/1234567890987654321234567890987654321)
             "#b10/111011011100010011100101100000010011000101110111101001110100010101110010000101001111011010110110001011000001110010110001"))))
(define-test sacla-must-printer.103 (:tag :sacla)
 (assert-true
  (let ((*print-base* 8))
    (string= (write-to-string 1/3) "1/3"))))
(define-test sacla-must-printer.104 (:tag :sacla)
 (assert-true
  (let ((*print-base* 8))
    (string= (write-to-string -1/4) "-1/4"))))
(define-test sacla-must-printer.105 (:tag :sacla)
 (assert-true
  (let ((*print-base* 8))
    (string= (write-to-string -3/7) "-3/7"))))
(define-test sacla-must-printer.106 (:tag :sacla)
 (assert-true
  (let ((*print-base* 8) (*print-radix* t))
    (string= (write-to-string 1/3) "#o1/3"))))
(define-test sacla-must-printer.107 (:tag :sacla)
 (assert-true
  (let ((*print-base* 8) (*print-radix* t))
    (string= (write-to-string -3/7) "#o-3/7"))))
(define-test sacla-must-printer.108 (:tag :sacla)
 (assert-true
  (let ((*print-base* 8) (*print-radix* t))
    (string= (write-to-string -15/11) "#o-17/13"))))
(define-test sacla-must-printer.109 (:tag :sacla)
 (assert-true
  (let ((*print-base* 8))
    (string= (write-to-string 2/1234567890987654321234567890987654321)
             "2/7334234540230567516425620517326613016261"))))
(define-test sacla-must-printer.110 (:tag :sacla)
 (assert-true
  (let ((*print-base* 8) (*print-radix* t))
    (string= (write-to-string -1234567890987654321234567890987654321/4)
             "#o-7334234540230567516425620517326613016261/4"))))
(define-test sacla-must-printer.111 (:tag :sacla)
 (assert-true
  (let ((*print-base* 16))
    (string= (write-to-string 1/8) "1/8"))))
(define-test sacla-must-printer.112 (:tag :sacla)
 (assert-true
  (let ((*print-base* 16))
    (string= (write-to-string -1/9) "-1/9"))))
(define-test sacla-must-printer.113 (:tag :sacla)
 (assert-true
  (let ((*print-base* 16))
    (string-equal (write-to-string -9/10) "-9/A"))))
(define-test sacla-must-printer.114 (:tag :sacla)
 (assert-true
  (let ((*print-base* 16) (*print-radix* t))
    (string= (write-to-string 1/3) "#x1/3"))))
(define-test sacla-must-printer.115 (:tag :sacla)
 (assert-true
  (let ((*print-base* 16) (*print-radix* t))
    (string= (write-to-string 3/8) "#x3/8"))))
(define-test sacla-must-printer.116 (:tag :sacla)
 (assert-true
  (let ((*print-base* 16) (*print-radix* t))
    (string= (write-to-string -4/9) "#x-4/9"))))
(define-test sacla-must-printer.117 (:tag :sacla)
 (assert-true
  (let ((*print-base* 16))
    (string= (write-to-string 2/1234567890987654321234567890987654321)
             "2/EDC4E5813177A7457214F6B62C1CB1"))))
(define-test sacla-must-printer.118 (:tag :sacla)
 (assert-true
  (let ((*print-base* 16) (*print-radix* t))
    (string-equal (write-to-string 1234567890987654321234567890987654321/4)
                  "#xEDC4E5813177A7457214F6B62C1CB1/4"))))
(define-test sacla-must-printer.119 (:tag :sacla)
 (assert-true
  (let ((*print-base* 16) (*print-radix* t))
    (string-equal (write-to-string 1234567890987654321234567890987654321/1234)
                  "#xEDC4E5813177A7457214F6B62C1CB1/4D2"))))
(define-test sacla-must-printer.120 (:tag :sacla)
 (assert-true
  (let ((*print-base* 21))
    (string= (write-to-string 1/8) "1/8"))))
(define-test sacla-must-printer.121 (:tag :sacla)
 (assert-true
  (let ((*print-base* 21))
    (string= (write-to-string -1/9) "-1/9"))))
(define-test sacla-must-printer.122 (:tag :sacla)
 (assert-true
  (let ((*print-base* 21))
    (string-equal (write-to-string -9/10) "-9/A"))))
(define-test sacla-must-printer.123 (:tag :sacla)
 (assert-true
  (let ((*print-base* 21) (*print-radix* t))
    (string= (write-to-string 1/4) "#21r1/4"))))
(define-test sacla-must-printer.124 (:tag :sacla)
 (assert-true
  (let ((*print-base* 21) (*print-radix* t))
    (string-equal (write-to-string -1/20) "#21r-1/K"))))
(define-test sacla-must-printer.125 (:tag :sacla)
 (assert-true
  (let ((*print-base* 21))
    (string= (write-to-string 2/1234567890987654321234567890987654321)
             "2/29FADE40CGDJK4D0654KEAD5K6EK"))))
(define-test sacla-must-printer.126 (:tag :sacla)
 (assert-true
  (let ((*print-base* 21) (*print-radix* t))
    (string-equal (write-to-string 1234567890987654321234567890987654321/1234)
                  "#21r29FADE40CGDJK4D0654KEAD5K6EK/2GG"))))
(define-test sacla-must-printer.127 (:tag :sacla)
 (assert-true
  (loop for *print-base* from 3 upto 36
        always (string= (write-to-string 1/2) "1/2"))))
(define-test sacla-must-printer.128 (:tag :sacla)
 (assert-true
  (loop for *print-base* from 4 upto 36
        always (string= (write-to-string -1/3) "-1/3"))))
(define-test sacla-must-printer.129 (:tag :sacla)
 (assert-true
  (loop for *print-base* from 3 upto 36
        always (string=
                (string-upcase (write-to-string (/ 1 (1- *print-base*))))
                (concatenate 'string
                             "1/"
                             (string
                              (char "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                    (1- *print-base*))))))))
(define-test sacla-must-printer.130 (:tag :sacla)
 (assert-true
  (loop for *print-base* from 2 upto 36
        always (string= (write-to-string (/ 1 *print-base*)) "1/10"))))
(define-test sacla-must-printer.131 (:tag :sacla)
 (assert-true
  (let ((list nil))
    (equal
     (dotimes (i 35 (reverse list))
       (let ((*print-base* (+ i 2)))
         (push (string-upcase (write-to-string 41/40)) list)))
     '("101001/101000" "1112/1111" "221/220" "131/130" "105/104" "56/55"
       "51/50" "45/44" "41/40" "38/37" "35/34" "32/31" "2D/2C" "2B/2A" "29/28"
       "27/26" "25/24" "23/22" "21/20" "1K/1J" "1J/1I" "1I/1H" "1H/1G" "1G/1F"
       "1F/1E" "1E/1D" "1D/1C" "1C/1B" "1B/1A" "1A/19" "19/18" "18/17" "17/16"
       "16/15" "15/14")))))
(define-test sacla-must-printer.132 (:tag :sacla)
 (assert-true
  (let ((list nil))
    (equal
     (dotimes (i 35 (reverse list))
       (let ((*print-base* (+ i 2)) (*print-radix* t))
         (push (string-upcase (write-to-string 41/40)) list)))
     '("#B101001/101000" "#3R1112/1111" "#4R221/220" "#5R131/130" "#6R105/104"
       "#7R56/55" "#O51/50" "#9R45/44" "#10R41/40" "#11R38/37" "#12R35/34"
       "#13R32/31" "#14R2D/2C" "#15R2B/2A" "#X29/28" "#17R27/26" "#18R25/24"
       "#19R23/22" "#20R21/20" "#21R1K/1J" "#22R1J/1I" "#23R1I/1H" "#24R1H/1G"
       "#25R1G/1F" "#26R1F/1E" "#27R1E/1D" "#28R1D/1C" "#29R1C/1B" "#30R1B/1A"
       "#31R1A/19" "#32R19/18" "#33R18/17" "#34R17/16" "#35R16/15"
       "#36R15/14")))))
(define-test sacla-must-printer.133 (:tag :sacla)
 (assert-true
  (let ((*print-escape* nil))
    (string= (write-to-string #\a) "a"))))
(define-test sacla-must-printer.134 (:tag :sacla)
 (assert-true
  (let ((*print-escape* nil) (*print-readably* nil))
    (string= (write-to-string #\d) "d"))))
(define-test sacla-must-printer.135 (:tag :sacla)
 (assert-true
  (let ((*print-escape* nil))
    (string= (write-to-string #\m) "m"))))
(define-test sacla-must-printer.136 (:tag :sacla)
 (assert-true
  (let ((*print-escape* nil))
    (string= (write-to-string #\z) "z"))))
(define-test sacla-must-printer.137 (:tag :sacla)
 (assert-true
  (let ((*print-escape* nil) (*print-readably* nil))
    (loop for c across
              " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_'abcdefghijklmnopqrstuvwxyz{|}~"
          always (string= (write-to-string c) (string c))))))
(define-test sacla-must-printer.138 (:tag :sacla)
 (assert-true
  (let ((*print-escape* nil))
    (loop for c across
              " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_'abcdefghijklmnopqrstuvwxyz{|}~"
          always (string= (write-to-string c) (string c))))))
(define-test sacla-must-printer.139 (:tag :sacla)
 (assert-true (string= (write-to-string #\b) "#\\b")))
(define-test sacla-must-printer.140 (:tag :sacla)
 (assert-true (string= (write-to-string #\n) "#\\n")))
(define-test sacla-must-printer.141 (:tag :sacla)
 (assert-true (string= (write-to-string #\x) "#\\x")))
(define-test sacla-must-printer.142 (:tag :sacla)
 (assert-true
  (let ((*print-escape* nil) (*print-readably* t))
    (string= (write-to-string #\c) "#\\c"))))
(define-test sacla-must-printer.143 (:tag :sacla)
 (assert-true
  (loop for c across
            "!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_'abcdefghijklmnopqrstuvwxyz{|}~"
        always (string= (write-to-string c)
                        (concatenate 'string "#\\" (string c))))))
(define-test sacla-must-printer.144 (:tag :sacla)
 (assert-true (string= (write-to-string #\\) "#\\\\")))
(define-test sacla-must-printer.145 (:tag :sacla)
 (assert-true (string= (write-to-string #\") "#\\\"")))
(define-test sacla-must-printer.146 (:tag :sacla)
 (assert-true
  (let ((*print-readably* t) (*print-escape* nil))
    (loop for c across
              "!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_'abcdefghijklmnopqrstuvwxyz{|}~"
          always (string= (write-to-string c)
                          (concatenate 'string "#\\" (string c)))))))
(define-test sacla-must-printer.147 (:tag :sacla)
 (assert-true
  (let ((*print-readably* t) (*print-escape* t))
    (loop for c across
              "!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_'abcdefghijklmnopqrstuvwxyz{|}~"
          always (string= (write-to-string c)
                          (concatenate 'string "#\\" (string c)))))))
(define-test sacla-must-printer.148 (:tag :sacla)
 (assert-true
  (let ((*print-readably* nil) (*print-escape* t))
    (loop for c across
              "!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_'abcdefghijklmnopqrstuvwxyz{|}~"
          always (string= (write-to-string c)
                          (concatenate 'string "#\\" (string c)))))))
(define-test sacla-must-printer.149 (:tag :sacla)
 (assert-true
  (progn
    (let ((*print-readably* t))
      (string= (write-to-string #\ ) "#\\ "))
    'skipped)))
(define-test sacla-must-printer.150 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :upcase)
    (string= "abc" (write-to-string '|abc| :escape nil :case :capitalize)))))
(define-test sacla-must-printer.151 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (string= "Abc" (write-to-string '|abc| :escape nil :case :capitalize)))))
(define-test sacla-must-printer.152 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (string= "abc" (write-to-string '|abc| :escape nil :case :capitalize)))))
(define-test sacla-must-printer.153 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :invert)
    (string= "ABC" (write-to-string '|abc| :escape nil :case :capitalize)))))
(define-test sacla-must-printer.154 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :upcase)
    (string= "Abc" (write-to-string 'abc :escape nil :case :capitalize)))))
(define-test sacla-must-printer.155 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (string= "ABC" (write-to-string 'abc :escape nil :case :capitalize)))))
(define-test sacla-must-printer.156 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (string= "ABC" (write-to-string 'abc :escape nil :case :capitalize)))))
(define-test sacla-must-printer.157 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :invert)
    (string= "abc" (write-to-string 'abc :escape nil :case :capitalize)))))
(define-test sacla-must-printer.158 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :upcase)
    (string= "Abc-abc"
             (write-to-string '|ABC-abc| :escape nil :case :capitalize)))))
(define-test sacla-must-printer.159 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (string= "ABC-Abc"
             (write-to-string '|ABC-abc| :escape nil :case :capitalize)))))
(define-test sacla-must-printer.160 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (string= "ABC-abc"
             (write-to-string '|ABC-abc| :escape nil :case :capitalize)))))
(define-test sacla-must-printer.161 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :invert)
    (string= "ABC-abc"
             (write-to-string '|ABC-abc| :escape nil :case :capitalize)))))
(define-test sacla-must-printer.162 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :upcase)
    (string= "abc-Abc"
             (write-to-string '|abc-ABC| :escape nil :case :capitalize)))))
(define-test sacla-must-printer.163 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (string= "Abc-ABC"
             (write-to-string '|abc-ABC| :escape nil :case :capitalize)))))
(define-test sacla-must-printer.164 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (string= "abc-ABC"
             (write-to-string '|abc-ABC| :escape nil :case :capitalize)))))
(define-test sacla-must-printer.165 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :invert)
    (string= "abc-ABC"
             (write-to-string '|abc-ABC| :escape nil :case :capitalize)))))
(define-test sacla-must-printer.166 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :upcase)
    (string= "abc" (write-to-string '|abc| :escape nil :case :upcase)))))
(define-test sacla-must-printer.167 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (string= "ABC" (write-to-string '|abc| :escape nil :case :upcase)))))
(define-test sacla-must-printer.168 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (string= "abc" (write-to-string '|abc| :escape nil :case :upcase)))))
(define-test sacla-must-printer.169 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :invert)
    (string= "ABC" (write-to-string '|abc| :escape nil :case :upcase)))))
(define-test sacla-must-printer.170 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :upcase)
    (string= "ABC" (write-to-string 'abc :escape nil :case :upcase)))))
(define-test sacla-must-printer.171 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (string= "ABC" (write-to-string 'abc :escape nil :case :upcase)))))
(define-test sacla-must-printer.172 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (string= "ABC" (write-to-string 'abc :escape nil :case :upcase)))))
(define-test sacla-must-printer.173 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :invert)
    (string= "abc" (write-to-string 'abc :escape nil :case :upcase)))))
(define-test sacla-must-printer.174 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :upcase)
    (string= "ABC-abc"
             (write-to-string '|ABC-abc| :escape nil :case :upcase)))))
(define-test sacla-must-printer.175 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (string= "ABC-ABC"
             (write-to-string '|ABC-abc| :escape nil :case :upcase)))))
(define-test sacla-must-printer.176 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (string= "ABC-abc"
             (write-to-string '|ABC-abc| :escape nil :case :upcase)))))
(define-test sacla-must-printer.177 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :invert)
    (string= "ABC-abc"
             (write-to-string '|ABC-abc| :escape nil :case :upcase)))))
(define-test sacla-must-printer.178 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :upcase)
    (string= "abc-ABC"
             (write-to-string '|abc-ABC| :escape nil :case :upcase)))))
(define-test sacla-must-printer.179 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (string= "ABC-ABC"
             (write-to-string '|abc-ABC| :escape nil :case :upcase)))))
(define-test sacla-must-printer.180 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (string= "abc-ABC"
             (write-to-string '|abc-ABC| :escape nil :case :upcase)))))
(define-test sacla-must-printer.181 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :invert)
    (string= "abc-ABC"
             (write-to-string '|abc-ABC| :escape nil :case :upcase)))))
(define-test sacla-must-printer.182 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :upcase)
    (string= "abc" (write-to-string '|abc| :escape nil :case :downcase)))))
(define-test sacla-must-printer.183 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (string= "abc" (write-to-string '|abc| :escape nil :case :downcase)))))
(define-test sacla-must-printer.184 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (string= "abc" (write-to-string '|abc| :escape nil :case :downcase)))))
(define-test sacla-must-printer.185 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :invert)
    (string= "ABC" (write-to-string '|abc| :escape nil :case :downcase)))))
(define-test sacla-must-printer.186 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :upcase)
    (string= "abc" (write-to-string 'abc :escape nil :case :downcase)))))
(define-test sacla-must-printer.187 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (string= "ABC" (write-to-string 'abc :escape nil :case :downcase)))))
(define-test sacla-must-printer.188 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (string= "ABC" (write-to-string 'abc :escape nil :case :downcase)))))
(define-test sacla-must-printer.189 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :invert)
    (string= "abc" (write-to-string 'abc :escape nil :case :downcase)))))
(define-test sacla-must-printer.190 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :upcase)
    (string= "abc-abc"
             (write-to-string '|ABC-abc| :escape nil :case :downcase)))))
(define-test sacla-must-printer.191 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (string= "ABC-abc"
             (write-to-string '|ABC-abc| :escape nil :case :downcase)))))
(define-test sacla-must-printer.192 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (string= "ABC-abc"
             (write-to-string '|ABC-abc| :escape nil :case :downcase)))))
(define-test sacla-must-printer.193 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :invert)
    (string= "ABC-abc"
             (write-to-string '|ABC-abc| :escape nil :case :downcase)))))
(define-test sacla-must-printer.194 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :upcase)
    (string= "abc-abc"
             (write-to-string '|abc-ABC| :escape nil :case :downcase)))))
(define-test sacla-must-printer.195 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (string= "abc-ABC"
             (write-to-string '|abc-ABC| :escape nil :case :downcase)))))
(define-test sacla-must-printer.196 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (string= "abc-ABC"
             (write-to-string '|abc-ABC| :escape nil :case :downcase)))))
(define-test sacla-must-printer.197 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :invert)
    (string= "abc-ABC"
             (write-to-string '|abc-ABC| :escape nil :case :downcase)))))
(define-test sacla-must-printer.198 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :upcase)
    (string= "abc-Abc"
             (write-to-string ':|abc-ABC| :escape nil :case :capitalize)))))
(define-test sacla-must-printer.199 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :upcase)
    (string= "abc-ABC"
             (write-to-string ':|abc-ABC| :escape nil :case :upcase)))))
(define-test sacla-must-printer.200 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :upcase)
    (string= "abc-abc"
             (write-to-string ':|abc-ABC| :escape nil :case :downcase)))))
(define-test sacla-must-printer.201 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (when (find-package "TEST-PKG0")
      (delete-package "TEST-PKG0"))
    (make-package "TEST-PKG0" :use nil)
    (setf (readtable-case *readtable*) :upcase)
    (string= "abc"
             (write-to-string (intern "abc" "TEST-PKG0")
                              :escape nil
                              :case :capitalize)))))
(define-test sacla-must-printer.202 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (when (find-package "TEST-PKG0")
      (delete-package "TEST-PKG0"))
    (make-package "TEST-PKG0" :use nil)
    (setf (readtable-case *readtable*) :upcase)
    (string= "abc"
             (write-to-string (intern "abc" "TEST-PKG0")
                              :escape nil
                              :case :upcase)))))
(define-test sacla-must-printer.203 (:tag :sacla)
 (assert-true
  (let ((*readtable* (copy-readtable nil)))
    (when (find-package "TEST-PKG0")
      (delete-package "TEST-PKG0"))
    (make-package "TEST-PKG0" :use nil)
    (setf (readtable-case *readtable*) :upcase)
    (string= "abc"
             (write-to-string (intern "abc" "TEST-PKG0")
                              :escape nil
                              :case :downcase)))))
(define-test sacla-must-printer.204 (:tag :sacla)
 (assert-true
  (loop named loop0
        with printed-name
        with *readtable* = (copy-readtable nil)
        for readtable-case in '(:upcase :downcase :preserve :invert)
        do (loop for *print-case* in '(:upcase :downcase :capitalize)
                 do (loop for symbol in '(zebra |Zebra| |zebra|)
                          do (setf (readtable-case *readtable*) readtable-case)
                             (setq printed-name
                                     (write-to-string symbol :readably t))
                          unless (eq symbol (read-from-string printed-name))
                            do (format t
                                       "~&Symbol = ~S~%Erroneous printed representation = ~S~%readtable-case = ~S~%*print-case* = ~S~%"
                                       symbol
                                       printed-name
                                       readtable-case
                                       *print-case*)
                               (return-from loop0 nil)))
        finally (return-from loop0 t))))
(define-test sacla-must-printer.205 (:tag :sacla)
 (assert-true
  (loop named loop0
        with printed-name
        with *readtable* = (copy-readtable nil)
        for readtable-case in '(:upcase :downcase :preserve :invert)
        do (loop for *print-case* in '(:upcase :downcase :capitalize)
                 do (loop for symbol in '(:zebra :|Zebra| :|zebra|)
                          do (setf (readtable-case *readtable*) readtable-case)
                             (setq printed-name
                                     (write-to-string symbol :readably t))
                          unless (eq symbol (read-from-string printed-name))
                            do (format t
                                       "~&Symbol = ~S~%Erroneous printed representation = ~S~%readtable-case = ~S~%*print-case* = ~S~%"
                                       symbol
                                       printed-name
                                       readtable-case
                                       *print-case*)
                               (return-from loop0 nil)))
        finally (return-from loop0 t))))
(define-test sacla-must-printer.206 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TEST-PKG0")
      (delete-package "TEST-PKG0"))
    (make-package "TEST-PKG0" :use nil)
    (loop named loop0
          with printed-name
          with *readtable* = (copy-readtable nil)
          for readtable-case in '(:upcase :downcase :preserve :invert)
          do (loop for *print-case* in '(:upcase :downcase :capitalize)
                   do (loop for symbol in
                                (mapcar
                                 #'(lambda (name) (intern name "TEST-PKG0"))
                                 '("ZEBRA" "Zebra" "zebra"))
                            do (setf (readtable-case *readtable*)
                                       readtable-case)
                               (setq printed-name
                                       (write-to-string symbol :readably t))
                            unless (eq symbol (read-from-string printed-name))
                              do (format t
                                         "~&Symbol = ~S~%Erroneous printed representation = ~S~%readtable-case = ~S~%*print-case* = ~S~%"
                                         symbol
                                         printed-name
                                         readtable-case
                                         *print-case*)
                                 (return-from loop0 nil)))
          finally (return-from loop0 t)))))
(define-test sacla-must-printer.207 (:tag :sacla)
 (assert-true (eq '|	| (read-from-string (write-to-string '|	| :readably t)))))
(define-test sacla-must-printer.208 (:tag :sacla)
 (assert-true
  (eq '|
|
      (read-from-string
       (write-to-string '|
|
                        :readably t)))))
(define-test sacla-must-printer.209 (:tag :sacla)
 (assert-true (eq '| | (read-from-string (write-to-string '| | :readably t)))))
(define-test sacla-must-printer.210 (:tag :sacla)
 (assert-true (eq '|"| (read-from-string (write-to-string '|"| :readably t)))))
(define-test sacla-must-printer.211 (:tag :sacla)
 (assert-true (eq '|#| (read-from-string (write-to-string '|#| :readably t)))))
(define-test sacla-must-printer.212 (:tag :sacla)
 (assert-true (eq '|'| (read-from-string (write-to-string '|'| :readably t)))))
(define-test sacla-must-printer.213 (:tag :sacla)
 (assert-true (eq '|(| (read-from-string (write-to-string '|(| :readably t)))))
(define-test sacla-must-printer.214 (:tag :sacla)
 (assert-true (eq '|)| (read-from-string (write-to-string '|)| :readably t)))))
(define-test sacla-must-printer.215 (:tag :sacla)
 (assert-true (eq '|,| (read-from-string (write-to-string '|,| :readably t)))))
(define-test sacla-must-printer.216 (:tag :sacla)
 (assert-true (eq '|;| (read-from-string (write-to-string '|;| :readably t)))))
(define-test sacla-must-printer.217 (:tag :sacla)
 (assert-true
  (eq '|\\| (read-from-string (write-to-string '|\\| :readably t)))))
(define-test sacla-must-printer.218 (:tag :sacla)
 (assert-true
  (= 1
     (length
      (symbol-name (read-from-string (write-to-string '|\\| :readably t)))))))
(define-test sacla-must-printer.219 (:tag :sacla)
 (assert-true (eq '|`| (read-from-string (write-to-string '|`| :readably t)))))
(define-test sacla-must-printer.220 (:tag :sacla)
 (assert-true
  (eq '|\|| (read-from-string (write-to-string '|\|| :readably t)))))
(define-test sacla-must-printer.221 (:tag :sacla)
 (assert-true
  (= 1
     (length
      (symbol-name (read-from-string (write-to-string '|\|| :readably t)))))))
(define-test sacla-must-printer.222 (:tag :sacla)
 (assert-true
  (loop for symbol in '(-!- /*/ $$$ ^^^^^^^^^^^^^)
        always (loop with *readtable* = (copy-readtable nil)
                     for table-case in '(:upcase :downcase :preserve :invert)
                     do (setf (readtable-case *readtable*) table-case)
                     always (loop for *print-case in
                                      '(:upcase :downcase :capitalize)
                                  always (string= (symbol-name symbol)
                                                  (write-to-string symbol
                                                                   :escape nil)))))))
(define-test sacla-must-printer.223 (:tag :sacla)
 (assert-true
  (string= "ABC"
           (symbol-name
            (read-from-string
             (write-to-string (make-symbol "ABC")
                              :readably t
                              :case :upcase))))))
(define-test sacla-must-printer.224 (:tag :sacla)
 (assert-true
  (string= "ABC"
           (symbol-name
            (read-from-string
             (write-to-string (make-symbol "ABC")
                              :readably t
                              :case :downcase))))))
(define-test sacla-must-printer.225 (:tag :sacla)
 (assert-true
  (string= "ABC"
           (symbol-name
            (read-from-string
             (write-to-string (make-symbol "ABC")
                              :readably t
                              :case :capitalize))))))
(define-test sacla-must-printer.226 (:tag :sacla)
 (assert-true
  (string= "G01" (write-to-string (make-symbol "G01") :escape t :gensym nil))))
(define-test sacla-must-printer.227 (:tag :sacla)
 (assert-true
  (string= "G01"
           (write-to-string (make-symbol "G01") :escape nil :gensym nil))))
(define-test sacla-must-printer.228 (:tag :sacla)
 (assert-true
  (string= "#:G01" (write-to-string (make-symbol "G01") :escape t :gensym t))))
(define-test sacla-must-printer.229 (:tag :sacla)
 (assert-true
  (string= "#:G01"
           (write-to-string (make-symbol "G01")
                            :escape nil
                            :gensym nil
                            :readably t))))
(define-test sacla-must-printer.230 (:tag :sacla)
 (assert-true
  (let ((face
         (let ((*print-base* 16))
           (write-to-string 'face :readably t)))
        (*read-base* 16))
    (eq 'face (read-from-string face)))))
(define-test sacla-must-printer.231 (:tag :sacla)
 (assert-true
  (eq '|01| (read-from-string (write-to-string '|01| :readably t)))))
(define-test sacla-must-printer.232 (:tag :sacla)
 (assert-true (eq '|1| (read-from-string (write-to-string '|1| :readably t)))))
(define-test sacla-must-printer.233 (:tag :sacla)
 (assert-true
  (eq '|0123456789|
      (read-from-string (write-to-string '|0123456789| :readably t)))))
(define-test sacla-must-printer.234 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "Test-Pkg0")
      (delete-package "Test-Pkg0"))
    (make-package "Test-Pkg0" :use nil)
    (loop named loop0
          with printed-name
          with *readtable* = (copy-readtable nil)
          for readtable-case in '(:upcase :downcase :preserve :invert)
          do (loop for *print-case* in '(:upcase :downcase :capitalize)
                   do (loop for symbol in
                                (mapcar
                                 #'(lambda (name) (intern name "Test-Pkg0"))
                                 '("ZEBRA" "Zebra" "zebra"))
                            do (setf (readtable-case *readtable*)
                                       readtable-case)
                               (setq printed-name
                                       (write-to-string symbol :readably t))
                            unless (eq symbol (read-from-string printed-name))
                              do (format t
                                         "~&Symbol = ~S~%Erroneous printed representation = ~S~%readtable-case = ~S~%*print-case* = ~S~%"
                                         symbol
                                         printed-name
                                         readtable-case
                                         *print-case*)
                                 (return-from loop0 nil)))
          finally (return-from loop0 t)))))
(define-test sacla-must-printer.235 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "Test|Pkg 0;")
      (delete-package "Test|Pkg 0;"))
    (make-package "Test|Pkg 0;" :use nil)
    (loop named loop0
          with *readtable* = (copy-readtable nil)
          for readtable-case in '(:upcase :downcase :preserve :invert)
          do (loop for *print-case* in '(:upcase :downcase :capitalize)
                   do (loop for symbol in
                                (mapcar
                                 #'(lambda (name) (intern name "Test|Pkg 0;"))
                                 '("ZEBRA" "Zebra" "zebra"))
                            do (setf (readtable-case *readtable*)
                                       readtable-case)
                            unless (eq symbol
                                       (read-from-string
                                        (write-to-string symbol :readably t)))
                              do (format t
                                         "~&Symbol = ~S~%Erroneous printed representation = ~S~%readtable-case = ~S~%*print-case* = ~S~%"
                                         symbol
                                         printed-name
                                         readtable-case
                                         *print-case*)
                                 (return-from loop0 nil)))
          finally (return-from loop0 t)))))
(define-test sacla-must-printer.236 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "Test|Pkg 0;")
      (delete-package "Test|Pkg 0;"))
    (make-package "Test|Pkg 0;" :use nil)
    (loop named loop0
          with *readtable* = (copy-readtable nil)
          for readtable-case in '(:upcase :downcase :preserve :invert)
          do (loop for *print-case* in '(:upcase :downcase :capitalize)
                   do (loop for symbol in
                                (mapcar
                                 #'(lambda (name) (intern name "Test|Pkg 0;"))
                                 '("Z\\E\"BRA" "Z;e|bra" "z:e bra"))
                            do (setf (readtable-case *readtable*)
                                       readtable-case)
                            unless (eq symbol
                                       (read-from-string
                                        (write-to-string symbol :readably t)))
                              do (format t
                                         "~&Symbol = ~S~%Erroneous printed representation = ~S~%readtable-case = ~S~%*print-case* = ~S~%"
                                         symbol
                                         printed-name
                                         readtable-case
                                         *print-case*)
                                 (return-from loop0 nil)))
          finally (return-from loop0 t)))))
(define-test sacla-must-printer.237 (:tag :sacla)
 (assert-true (string= "#*0101" (write-to-string #*0101 :readably t :array t))))
(define-test sacla-must-printer.238 (:tag :sacla)
 (assert-true (string= "#*01" (write-to-string #*01 :readably t :array t))))
(define-test sacla-must-printer.239 (:tag :sacla)
 (assert-true (string= "#*0" (write-to-string #*0 :readably t :array t))))
(define-test sacla-must-printer.240 (:tag :sacla)
 (assert-true (string= "#*1" (write-to-string #*1 :readably t :array t))))
(define-test sacla-must-printer.241 (:tag :sacla)
 (assert-true (string= "#*" (write-to-string #* :readably t :array t))))
(define-test sacla-must-printer.242 (:tag :sacla)
 (assert-true
  (string= "#*10101111000"
           (write-to-string #*10101111000 :readably t :array t))))
(define-test sacla-must-printer.243 (:tag :sacla)
 (assert-true
  (string= "#*0101" (write-to-string #*0101 :readably t :array nil))))
(define-test sacla-must-printer.244 (:tag :sacla)
 (assert-true (string= "#*01" (write-to-string #*01 :readably t :array nil))))
(define-test sacla-must-printer.245 (:tag :sacla)
 (assert-true (string= "#*0" (write-to-string #*0 :readably t :array nil))))
(define-test sacla-must-printer.246 (:tag :sacla)
 (assert-true (string= "#*1" (write-to-string #*1 :readably t :array nil))))
(define-test sacla-must-printer.247 (:tag :sacla)
 (assert-true (string= "#*" (write-to-string #* :readably t :array nil))))
(define-test sacla-must-printer.248 (:tag :sacla)
 (assert-true
  (string= "#*10101111000"
           (write-to-string #*10101111000 :readably t :array nil))))
(define-test sacla-must-printer.249 (:tag :sacla)
 (assert-true (string= "#*0101" (write-to-string #*0101 :array t))))
(define-test sacla-must-printer.250 (:tag :sacla)
 (assert-true (string= "#*01" (write-to-string #*01 :array t))))
(define-test sacla-must-printer.251 (:tag :sacla)
 (assert-true (string= "#*0" (write-to-string #*0 :array t))))
(define-test sacla-must-printer.252 (:tag :sacla)
 (assert-true (string= "#*1" (write-to-string #*1 :array t))))
(define-test sacla-must-printer.253 (:tag :sacla)
 (assert-true (string= "#*" (write-to-string #* :array t))))
(define-test sacla-must-printer.254 (:tag :sacla)
 (assert-true
  (string= "#*10101111000" (write-to-string #*10101111000 :array t))))
(define-test sacla-must-printer.255 (:tag :sacla)
 (assert-true (zerop (search "#<" (write-to-string #*0101 :array nil)))))
(define-test sacla-must-printer.256 (:tag :sacla)
 (assert-true (zerop (search "#<" (write-to-string #*01 :array nil)))))
(define-test sacla-must-printer.257 (:tag :sacla)
 (assert-true (zerop (search "#<" (write-to-string #*0 :array nil)))))
(define-test sacla-must-printer.258 (:tag :sacla)
 (assert-true (zerop (search "#<" (write-to-string #*1 :array nil)))))
(define-test sacla-must-printer.259 (:tag :sacla)
 (assert-true (zerop (search "#<" (write-to-string #* :array nil)))))
(define-test sacla-must-printer.260 (:tag :sacla)
 (assert-true (zerop (search "#<" (write-to-string #*10101111000 :array nil)))))
(define-test sacla-must-printer.261 (:tag :sacla)
 (assert-true
  (string= "#*01"
           (write-to-string
            (make-array 10
                        :element-type 'bit
                        :initial-contents '(0 1 0 1 0 1 0 1 0 1)
                        :fill-pointer 2)
            :readably t
            :array t))))
(define-test sacla-must-printer.262 (:tag :sacla)
 (assert-true (null (read-from-string (write-to-string 'nil)))))
(define-test sacla-must-printer.263 (:tag :sacla)
 (assert-true (string= (write-to-string '(1) :pretty nil) "(1)")))
(define-test sacla-must-printer.264 (:tag :sacla)
 (assert-true (string= (write-to-string '(1 2) :pretty nil) "(1 2)")))
(define-test sacla-must-printer.265 (:tag :sacla)
 (assert-true (string= (write-to-string '(1 2 3) :pretty nil) "(1 2 3)")))
(define-test sacla-must-printer.266 (:tag :sacla)
 (assert-true (string= (write-to-string '(1 2 3 4) :pretty nil) "(1 2 3 4)")))
(define-test sacla-must-printer.267 (:tag :sacla)
 (assert-true (string= (write-to-string '(1 . 2) :pretty nil) "(1 . 2)")))
(define-test sacla-must-printer.268 (:tag :sacla)
 (assert-true (string= (write-to-string '(1 2 . 3) :pretty nil) "(1 2 . 3)")))
(define-test sacla-must-printer.269 (:tag :sacla)
 (assert-true
  (string= (write-to-string '(1 2 3 . 4) :pretty nil) "(1 2 3 . 4)")))
(define-test sacla-must-printer.270 (:tag :sacla)
 (assert-true
  (let ((list (loop for i from 0 upto 100 collect i)))
    (equal (read-from-string (write-to-string list)) list))))
(define-test sacla-must-printer.271 (:tag :sacla)
 (assert-true
  (string= (write-to-string '(1 (2 (3 (4 (5 (6)))))) :pretty nil :level 0)
           "#")))
(define-test sacla-must-printer.272 (:tag :sacla)
 (assert-true
  (string= (write-to-string '(1 (2 (3 (4 (5 (6)))))) :pretty nil :level 1)
           "(1 #)")))
(define-test sacla-must-printer.273 (:tag :sacla)
 (assert-true
  (string= (write-to-string '(1 (2 (3 (4 (5 (6)))))) :pretty nil :level 2)
           "(1 (2 #))")))
(define-test sacla-must-printer.274 (:tag :sacla)
 (assert-true
  (string= (write-to-string '(1 (2 (3 (4 (5 (6)))))) :pretty nil :level 3)
           "(1 (2 (3 #)))")))
(define-test sacla-must-printer.275 (:tag :sacla)
 (assert-true
  (string= (write-to-string '(1 (2 (3 (4 (5 (6)))))) :pretty nil :level 4)
           "(1 (2 (3 (4 #))))")))
(define-test sacla-must-printer.276 (:tag :sacla)
 (assert-true
  (string= (write-to-string '(1 (2 (3 (4 (5 (6)))))) :pretty nil :level 4)
           "(1 (2 (3 (4 #))))")))
(define-test sacla-must-printer.277 (:tag :sacla)
 (assert-true
  (string= (write-to-string '(1 (2 (3 (4 (5 (6)))))) :pretty nil :level 5)
           "(1 (2 (3 (4 (5 #)))))")))
(define-test sacla-must-printer.278 (:tag :sacla)
 (assert-true
  (string= (write-to-string '(1 (2 (3 (4 (5 (6)))))) :pretty nil :level 6)
           "(1 (2 (3 (4 (5 (6))))))")))
(define-test sacla-must-printer.279 (:tag :sacla)
 (assert-true
  (string= (write-to-string '(1 (2 (3 (4 (5 (6)))))) :pretty nil :level 7)
           "(1 (2 (3 (4 (5 (6))))))")))
(define-test sacla-must-printer.280 (:tag :sacla)
 (assert-true
  (string= (write-to-string '(1 (2 (3 (4 (5 (6)))))) :pretty nil :level 100)
           "(1 (2 (3 (4 (5 (6))))))")))
(define-test sacla-must-printer.281 (:tag :sacla)
 (assert-true
  (string= (write-to-string '(1 2 3 4 5 6) :pretty nil :length 0) "(...)")))
(define-test sacla-must-printer.282 (:tag :sacla)
 (assert-true
  (string= (write-to-string '(1 2 3 4 5 6) :pretty nil :length 1) "(1 ...)")))
(define-test sacla-must-printer.283 (:tag :sacla)
 (assert-true
  (string= (write-to-string '(1 2 3 4 5 6) :pretty nil :length 2) "(1 2 ...)")))
(define-test sacla-must-printer.284 (:tag :sacla)
 (assert-true
  (string= (write-to-string '(1 2 3 4 5 6) :pretty nil :length 3)
           "(1 2 3 ...)")))
(define-test sacla-must-printer.285 (:tag :sacla)
 (assert-true
  (string= (write-to-string '(1 2 3 4 5 6) :pretty nil :length 4)
           "(1 2 3 4 ...)")))
(define-test sacla-must-printer.286 (:tag :sacla)
 (assert-true
  (string= (write-to-string '(1 2 3 4 5 6) :pretty nil :length 5)
           "(1 2 3 4 5 ...)")))
(define-test sacla-must-printer.287 (:tag :sacla)
 (assert-true
  (string= (write-to-string '(1 2 3 4 5 6) :pretty nil :length 6)
           "(1 2 3 4 5 6)")))
(define-test sacla-must-printer.288 (:tag :sacla)
 (assert-true
  (string= (write-to-string '(1 2 3 4 5 6) :pretty nil :length 7)
           "(1 2 3 4 5 6)")))
(define-test sacla-must-printer.289 (:tag :sacla)
 (assert-true
  (string= (write-to-string '(1 2 3 4 5 6) :pretty nil :length 100)
           "(1 2 3 4 5 6)")))
(define-test sacla-must-printer.290 (:tag :sacla)
 (assert-true
  (string= (write-to-string '(1 2 . 3) :pretty nil :length 0) "(...)")))
(define-test sacla-must-printer.291 (:tag :sacla)
 (assert-true
  (string= (write-to-string '(1 2 . 3) :pretty nil :length 1) "(1 ...)")))
(define-test sacla-must-printer.292 (:tag :sacla)
 (assert-true
  (string= (write-to-string '(1 2 . 3) :pretty nil :length 2) "(1 2 . 3)")))
(define-test sacla-must-printer.293 (:tag :sacla)
 (assert-true
  (string= (write-to-string '(1 2 . 3) :pretty nil :length 3) "(1 2 . 3)")))
(define-test sacla-must-printer.294 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '(1 (2 (3 (4 (5 (6)))))) :pretty nil :level 0 :length 0)
   "#")))
(define-test sacla-must-printer.295 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '(1 (2 (3 (4 (5 (6)))))) :pretty nil :level 1 :length 0)
   "(...)")))
(define-test sacla-must-printer.296 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '(1 (2 (3 (4 (5 (6)))))) :pretty nil :level 0 :length 1)
   "#")))
(define-test sacla-must-printer.297 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '(1 (2 (3 (4 (5 (6)))))) :pretty nil :level 1 :length 1)
   "(1 ...)")))
(define-test sacla-must-printer.298 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '(1 (2 (3 (4 (5 (6)))))) :pretty nil :level 2 :length 1)
   "(1 ...)")))
(define-test sacla-must-printer.299 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '(1 (2 (3 (4 (5 (6)))))) :pretty nil :level 2 :length 2)
   "(1 (2 #))")))
(define-test sacla-must-printer.300 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '((((1))) ((2)) (3) 4) :pretty nil :level 0 :length 0)
   "#")))
(define-test sacla-must-printer.301 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '((((1))) ((2)) (3) 4) :pretty nil :level 1 :length 0)
   "(...)")))
(define-test sacla-must-printer.302 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '((((1))) ((2)) (3) 4) :pretty nil :level 1 :length 4)
   "(# # # 4)")))
(define-test sacla-must-printer.303 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '((((1))) ((2)) (3) 4) :pretty nil :level 2 :length 3)
   "((#) (#) (3) ...)")))
(define-test sacla-must-printer.304 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '((((1))) ((2)) (3) 4) :pretty nil :level 3 :length 3)
   "(((#)) ((2)) (3) ...)")))
(define-test sacla-must-printer.305 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '((((1))) ((2)) (3) 4) :pretty nil :level 4 :length 3)
   "((((1))) ((2)) (3) ...)")))
(define-test sacla-must-printer.306 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '((((1))) ((2)) (3) 4) :pretty nil :level 2 :length 4)
   "((#) (#) (3) 4)")))
(define-test sacla-must-printer.307 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '((((1))) ((2)) (3) 4) :pretty nil :level 4 :length 4)
   "((((1))) ((2)) (3) 4)")))
(define-test sacla-must-printer.308 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '((((1))) ((2)) (3) 4 (5) ((6)) (((7))))
                    :pretty nil
                    :level 3
                    :length 6)
   "(((#)) ((2)) (3) 4 (5) ((6)) ...)")))
(define-test sacla-must-printer.309 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '((((1 ((2)) (3)))) ((2 (3) 4 5 6)) (3 (4 (5 6))))
                    :pretty nil
                    :level 6
                    :length 3)
   "((((1 ((2)) (3)))) ((2 (3) 4 ...)) (3 (4 (5 6))))")))
(define-test sacla-must-printer.310 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '((((1 ((2)) (3)))) ((2 (3) 4 5 6)) (3 (4 (5 6))))
                    :pretty nil
                    :level 2
                    :length 2)
   "((#) (#) ...)")))
(define-test sacla-must-printer.311 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '((((1 ((2)) (3)))) ((2 (3) 4 5 6)) (3 (4 (5 6))))
                    :pretty nil
                    :level 3
                    :length 2)
   "(((#)) ((2 # ...)) ...)")))
(define-test sacla-must-printer.312 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '(((1)) ((1) 2 ((3)) (((4)))) 3 (4))
                    :pretty nil
                    :level 2
                    :length 3)
   "((#) (# 2 # ...) 3 ...)")))
(define-test sacla-must-printer.313 (:tag :sacla)
 (assert-true (string= (write-to-string '#() :pretty nil :array t) "#()")))
(define-test sacla-must-printer.314 (:tag :sacla)
 (assert-true (string= (write-to-string '#(1) :pretty nil :array t) "#(1)")))
(define-test sacla-must-printer.315 (:tag :sacla)
 (assert-true
  (string= (write-to-string '#(1 2 3) :pretty nil :array t) "#(1 2 3)")))
(define-test sacla-must-printer.316 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string
    (make-array 10 :initial-contents '(0 1 2 3 4 5 6 7 8 9) :fill-pointer 3)
    :pretty nil
    :array t)
   "#(0 1 2)")))
(define-test sacla-must-printer.317 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '#(1 (2 (3 (4 (5 (6)))))) :pretty nil :array t :level 0)
   "#")))
(define-test sacla-must-printer.318 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '#(1 (2 (3 (4 (5 (6)))))) :pretty nil :array t :level 1)
   "#(1 #)")))
(define-test sacla-must-printer.319 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '#(1 (2 (3 (4 (5 (6)))))) :pretty nil :array t :level 2)
   "#(1 (2 #))")))
(define-test sacla-must-printer.320 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '#(1 (2 (3 (4 (5 (6)))))) :pretty nil :array t :level 3)
   "#(1 (2 (3 #)))")))
(define-test sacla-must-printer.321 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '#(1 (2 (3 (4 (5 (6)))))) :pretty nil :array t :level 4)
   "#(1 (2 (3 (4 #))))")))
(define-test sacla-must-printer.322 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '#(1 (2 (3 (4 (5 (6)))))) :pretty nil :array t :level 4)
   "#(1 (2 (3 (4 #))))")))
(define-test sacla-must-printer.323 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '#(1 (2 (3 (4 (5 (6)))))) :pretty nil :array t :level 5)
   "#(1 (2 (3 (4 (5 #)))))")))
(define-test sacla-must-printer.324 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '#(1 (2 (3 (4 (5 (6)))))) :pretty nil :array t :level 6)
   "#(1 (2 (3 (4 (5 (6))))))")))
(define-test sacla-must-printer.325 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '#(1 (2 (3 (4 (5 (6)))))) :pretty nil :array t :level 7)
   "#(1 (2 (3 (4 (5 (6))))))")))
(define-test sacla-must-printer.326 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '#(1 (2 (3 (4 (5 (6)))))) :pretty nil :array t :level 100)
   "#(1 (2 (3 (4 (5 (6))))))")))
(define-test sacla-must-printer.327 (:tag :sacla)
 (assert-true
  (string= (write-to-string '#(1 2 3 4 5 6) :pretty nil :array t :length 0)
           "#(...)")))
(define-test sacla-must-printer.328 (:tag :sacla)
 (assert-true
  (string= (write-to-string '#(1 2 3 4 5 6) :pretty nil :array t :length 1)
           "#(1 ...)")))
(define-test sacla-must-printer.329 (:tag :sacla)
 (assert-true
  (string= (write-to-string '#(1 2 3 4 5 6) :pretty nil :array t :length 2)
           "#(1 2 ...)")))
(define-test sacla-must-printer.330 (:tag :sacla)
 (assert-true
  (string= (write-to-string '#(1 2 3 4 5 6) :pretty nil :array t :length 3)
           "#(1 2 3 ...)")))
(define-test sacla-must-printer.331 (:tag :sacla)
 (assert-true
  (string= (write-to-string '#(1 2 3 4 5 6) :pretty nil :array t :length 4)
           "#(1 2 3 4 ...)")))
(define-test sacla-must-printer.332 (:tag :sacla)
 (assert-true
  (string= (write-to-string '#(1 2 3 4 5 6) :pretty nil :array t :length 5)
           "#(1 2 3 4 5 ...)")))
(define-test sacla-must-printer.333 (:tag :sacla)
 (assert-true
  (string= (write-to-string '#(1 2 3 4 5 6) :pretty nil :array t :length 6)
           "#(1 2 3 4 5 6)")))
(define-test sacla-must-printer.334 (:tag :sacla)
 (assert-true
  (string= (write-to-string '#(1 2 3 4 5 6) :pretty nil :array t :length 7)
           "#(1 2 3 4 5 6)")))
(define-test sacla-must-printer.335 (:tag :sacla)
 (assert-true
  (string= (write-to-string '#(1 2 3 4 5 6) :pretty nil :array t :length 100)
           "#(1 2 3 4 5 6)")))
(define-test sacla-must-printer.336 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '#(1 #(2 #(3 #(4 #(5 #(6))))))
                    :pretty nil
                    :array t
                    :level 0
                    :length 0)
   "#")))
(define-test sacla-must-printer.337 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '#(1 #(2 #(3 #(4 #(5 #(6))))))
                    :pretty nil
                    :array t
                    :level 1
                    :length 0)
   "#(...)")))
(define-test sacla-must-printer.338 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '#(1 #(2 #(3 #(4 #(5 #(6))))))
                    :pretty nil
                    :array t
                    :level 0
                    :length 1)
   "#")))
(define-test sacla-must-printer.339 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '#(1 #(2 #(3 #(4 #(5 #(6))))))
                    :pretty nil
                    :array t
                    :level 1
                    :length 1)
   "#(1 ...)")))
(define-test sacla-must-printer.340 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '#(1 #(2 #(3 #(4 #(5 #(6))))))
                    :pretty nil
                    :array t
                    :level 2
                    :length 1)
   "#(1 ...)")))
(define-test sacla-must-printer.341 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '#(1 #(2 #(3 #(4 #(5 #(6))))))
                    :pretty nil
                    :array t
                    :level 2
                    :length 2)
   "#(1 #(2 #))")))
(define-test sacla-must-printer.342 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '#(#(#(#(1))) #(#(2)) #(3) 4)
                    :pretty nil
                    :array t
                    :level 0
                    :length 0)
   "#")))
(define-test sacla-must-printer.343 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '#(#(#(#(1))) #(#(2)) #(3) 4)
                    :pretty nil
                    :array t
                    :level 1
                    :length 0)
   "#(...)")))
(define-test sacla-must-printer.344 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '#(#(#(#(1))) #(#(2)) #(3) 4)
                    :pretty nil
                    :array t
                    :level 1
                    :length 4)
   "#(# # # 4)")))
(define-test sacla-must-printer.345 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '#(#(#(#(1))) #(#(2)) #(3) 4)
                    :pretty nil
                    :array t
                    :level 2
                    :length 3)
   "#(#(#) #(#) #(3) ...)")))
(define-test sacla-must-printer.346 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '#(#(#(#(1))) #(#(2)) #(3) 4)
                    :pretty nil
                    :array t
                    :level 3
                    :length 3)
   "#(#(#(#)) #(#(2)) #(3) ...)")))
(define-test sacla-must-printer.347 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '#(#(#(#(1))) #(#(2)) #(3) 4)
                    :pretty nil
                    :array t
                    :level 4
                    :length 3)
   "#(#(#(#(1))) #(#(2)) #(3) ...)")))
(define-test sacla-must-printer.348 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '#(#(#(#(1))) #(#(2)) #(3) 4)
                    :pretty nil
                    :array t
                    :level 2
                    :length 4)
   "#(#(#) #(#) #(3) 4)")))
(define-test sacla-must-printer.349 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '#(#(#(#(1))) #(#(2)) #(3) 4)
                    :pretty nil
                    :array t
                    :level 4
                    :length 4)
   "#(#(#(#(1))) #(#(2)) #(3) 4)")))
(define-test sacla-must-printer.350 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '#(#(#(#(1))) #(#(2)) #(3) 4 #(5) #(#(6)) #(#(#(7))))
                    :pretty nil
                    :array t
                    :level 3
                    :length 6)
   "#(#(#(#)) #(#(2)) #(3) 4 #(5) #(#(6)) ...)")))
(define-test sacla-must-printer.351 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string
    '#(#(#(#(1 #(#(2)) #(3)))) #(#(2 #(3) 4 5 6)) #(3 #(4 #(5 6))))
    :pretty nil
    :array t
    :level 6
    :length 3)
   "#(#(#(#(1 #(#(2)) #(3)))) #(#(2 #(3) 4 ...)) #(3 #(4 #(5 6))))")))
(define-test sacla-must-printer.352 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string
    '#(#(#(#(1 #(#(2)) #(3)))) #(#(2 #(3) 4 5 6)) #(3 #(4 #(5 6))))
    :pretty nil
    :array t
    :level 2
    :length 2)
   "#(#(#) #(#) ...)")))
(define-test sacla-must-printer.353 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string
    '#(#(#(#(1 #(#(2)) #(3)))) #(#(2 #(3) 4 5 6)) #(3 #(4 #(5 6))))
    :pretty nil
    :array t
    :level 3
    :length 2)
   "#(#(#(#)) #(#(2 # ...)) ...)")))
(define-test sacla-must-printer.354 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '#(#(#(1)) #(#(1) 2 #(#(3)) #(#(#(4)))) 3 #(4))
                    :pretty nil
                    :array t
                    :level 2
                    :length 3)
   "#(#(#) #(# 2 # ...) 3 ...)")))
(define-test sacla-must-printer.355 (:tag :sacla)
 (assert-true (string= (write-to-string '#0A1 :pretty nil :array t) "#0A1")))
(define-test sacla-must-printer.356 (:tag :sacla)
 (assert-true (string= (write-to-string '#() :pretty nil :array t) "#()")))
(define-test sacla-must-printer.357 (:tag :sacla)
 (assert-true
  (string= (write-to-string '#(1 2 3) :pretty nil :array t) "#(1 2 3)")))
(define-test sacla-must-printer.358 (:tag :sacla)
 (assert-true
  (string= (write-to-string '#2A((1 2 3) (4 5 6)) :pretty nil :array t)
           "#2A((1 2 3) (4 5 6))")))
(define-test sacla-must-printer.359 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '#3A(((1 a) (2 b) (3 c)) ((4 d) (5 e) (6 f)))
                    :pretty nil
                    :array t)
   "#3A(((1 A) (2 B) (3 C)) ((4 D) (5 E) (6 F)))")))
(define-test sacla-must-printer.360 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string
    (make-array (make-list 20 :initial-element 1) :initial-element 0)
    :pretty nil
    :array t)
   "#20A((((((((((((((((((((0))))))))))))))))))))")))
(define-test sacla-must-printer.361 (:tag :sacla)
 (assert-true
  (string= (write-to-string '#0A10 :pretty nil :array t :level 1 :length 1)
           "#0A10")))
(define-test sacla-must-printer.362 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '#2A((0) (1) (2) (3))
                    :pretty nil
                    :array t
                    :level 1
                    :length 1)
   "#2A(# ...)")))
(define-test sacla-must-printer.363 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '#2A((0) (1) (2) (3))
                    :pretty nil
                    :array t
                    :level 2
                    :length 2)
   "#2A((0) (1) ...)")))
(define-test sacla-must-printer.364 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '#2A((0) (1) (2) (3))
                    :pretty nil
                    :array t
                    :level 2
                    :length 0)
   "#2A(...)")))
(define-test sacla-must-printer.365 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '#3A(((0) (1) (2)) ((3) (4) (5)))
                    :pretty nil
                    :array t
                    :level 3
                    :length 2)
   "#3A(((0) (1) ...) ((3) (4) ...))")))
(define-test sacla-must-printer.366 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string
    (make-array (make-list 20 :initial-element 1) :initial-element 0)
    :pretty nil
    :array t
    :level 0
    :length 100)
   "#")))
(define-test sacla-must-printer.367 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string
    (make-array (make-list 20 :initial-element 1) :initial-element 0)
    :pretty nil
    :array t
    :level 100
    :length 0)
   "#20A(...)")))
(define-test sacla-must-printer.368 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string
    (make-array (make-list 20 :initial-element 1) :initial-element 0)
    :pretty nil
    :array t
    :level 10
    :length 100)
   "#20A((((((((((#))))))))))")))
(define-test sacla-must-printer.369 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '#2A((0 1 2) (3 4 5) (6 7 8) (9 10 11))
                    :pretty nil
                    :array t
                    :level 2
                    :length 2)
   "#2A((0 1 ...) (3 4 ...) ...)")))
(define-test sacla-must-printer.370 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string '#2A((0 1 2) (3 4 5) (6 7 8) (9 10 11))
                    :pretty nil
                    :array t
                    :level 1
                    :length 2)
   "#2A(# # ...)")))
(define-test sacla-must-printer.371 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string
    '#3A(((0) (1) (2)) ((3) (4) (5)) ((6) (7) (8)) ((9) (10) (11)))
    :pretty nil
    :array t
    :level 2
    :length 3)
   "#3A((# # #) (# # #) (# # #) ...)")))
(define-test sacla-must-printer.372 (:tag :sacla)
 (assert-true
  (string=
   (write-to-string
    '#3A(((0) (1) (2)) ((3) (4) (5)) ((6) (7) (8)) ((9) (10) (11)))
    :pretty nil
    :array t
    :level 3
    :length 4)
   "#3A(((0) (1) (2)) ((3) (4) (5)) ((6) (7) (8)) ((9) (10) (11)))")))
(define-test sacla-must-printer.373 (:tag :sacla)
 (assert-true (string= (write-to-string "abc" :array t :escape nil) "abc")))
(define-test sacla-must-printer.374 (:tag :sacla)
 (assert-true (string= (write-to-string "abc" :array nil :escape nil) "abc")))
(define-test sacla-must-printer.375 (:tag :sacla)
 (assert-true (= 2 (mismatch "#<" (write-to-string #() :array nil)))))
(define-test sacla-must-printer.376 (:tag :sacla)
 (assert-true (= 2 (mismatch "#<" (write-to-string #(1 2 3) :array nil)))))
(define-test sacla-must-printer.377 (:tag :sacla)
 (assert-true (= 2 (mismatch "#<" (write-to-string #*1010 :array nil)))))
(define-test sacla-must-printer.378 (:tag :sacla)
 (assert-true
  (= 2 (mismatch "#<" (write-to-string #2A((0 1 2) (3 4 5)) :array nil)))))
(define-test sacla-must-printer.379 (:tag :sacla)
 (assert-true
  (= 2
     (mismatch "#<"
               (write-to-string #3A(((0 1) (2 3)) ((4 5) (6 7))) :array nil)))))
(define-test sacla-must-printer.380 (:tag :sacla)
 (assert-true
  (= 2
     (mismatch "#<"
               (write-to-string
                #4A((((0) (1)) ((2) (3)))
                    (((4) (5)) ((6) (7)))
                    (((8) (9)) ((10) (11)))
                    (((12) (13)) ((14) (15))))
                :array nil)))))
(define-test sacla-must-printer.381 (:tag :sacla)
 (assert-true
  (let* ((list '#1=(#1# . #1#))
         (x (read-from-string (write-to-string list :circle t))))
    (and (eq x (car x)) (eq x (cdr x))))))
(define-test sacla-must-printer.382 (:tag :sacla)
 (assert-true
  (let* ((list '#1=(a . #1#))
         (x (read-from-string (write-to-string list :circle t))))
    (and (eq (car x) 'a) (eq x (cdr x))))))
(define-test sacla-must-printer.383 (:tag :sacla)
 (assert-true
  (let* ((list '(a . #1=(b c . #1#)))
         (x (read-from-string (write-to-string list :circle t))))
    (and (eq (first x) 'a)
         (eq (second x) 'b)
         (eq (third x) 'c)
         (eq (fourth x) 'b)
         (eq (cdr x) (nthcdr 3 x))))))
(define-test sacla-must-printer.384 (:tag :sacla)
 (assert-true
  (let* ((list '(#1=#:g1041 #1#))
         (x (read-from-string (write-to-string list :circle t))))
    (and (= 2 (length x)) (symbolp (first x)) (eq (first x) (second x))))))
(define-test sacla-must-printer.385 (:tag :sacla)
 (assert-true
  (let* ((list '#1=(a (b #2=(x y z) . #1#) . #2#))
         (x (read-from-string (write-to-string list :circle t))))
    (and (eq (first x) 'a)
         (eq x (cddr (second x)))
         (eq (second (second x)) (cddr x))))))
(define-test sacla-must-printer.386 (:tag :sacla)
 (assert-true
  (let* ((list '#1=#(#1# a))
         (x (read-from-string (write-to-string list :circle t))))
    (and (eq x (aref x 0)) (eq 'a (aref x 1))))))
(define-test sacla-must-printer.387 (:tag :sacla)
 (assert-true
  (let* ((list '#1=#(a #1#))
         (x (read-from-string (write-to-string list :circle t))))
    (and (eq (aref x 0) 'a) (eq x (aref x 1))))))
(define-test sacla-must-printer.388 (:tag :sacla)
 (assert-true
  (let* ((list '#(#1=#:g00 #1#))
         (x (read-from-string (write-to-string list :circle t))))
    (and (eq (aref x 0) (aref x 1))
         (string= (symbol-name (aref x 0)) "G00")
         (null (symbol-package (aref x 0)))))))
(define-test sacla-must-printer.389 (:tag :sacla)
 (assert-true
  (let* ((list '#(#(#1=#:g00) #2=#(#1# a) #(#2# #1#)))
         (x (read-from-string (write-to-string list :circle t))))
    (and (= 3 (length x))
         (= 1 (length (aref x 0)))
         (= 2 (length (aref x 1)))
         (= 2 (length (aref x 2)))
         (eq (aref (aref x 0) 0) (aref (aref x 1) 0))
         (eq 'a (aref (aref x 1) 1))
         (eq (aref (aref x 0) 0) (aref (aref x 2) 1))
         (eq (aref x 1) (aref (aref x 2) 0))))))
(define-test sacla-must-printer.390 (:tag :sacla)
 (assert-true
  (let* ((array '#1=#0A#1#)
         (x (read-from-string (write-to-string array :array t :circle t))))
    (and (null (array-dimensions array)) (eq x (aref x))))))
(define-test sacla-must-printer.391 (:tag :sacla)
 (assert-true
  (let* ((array '#1=#2A((1 2 3) (4 5 #1#)))
         (x (read-from-string (write-to-string array :array t :circle t))))
    (and (equal (array-dimensions array) '(2 3))
         (= 1 (aref x 0 0))
         (= 2 (aref x 0 1))
         (= 3 (aref x 0 2))
         (= 4 (aref x 1 0))
         (= 5 (aref x 1 1))
         (eq x (aref x 1 2))))))
(define-test sacla-must-printer.392 (:tag :sacla)
 (assert-true
  (let* ((array #1=#3A(((1 a) (2 b) (3 #1#)) ((4 d) (5 e) (6 f))))
         (x (read-from-string (write-to-string array :array t :circle t))))
    (and (equal (array-dimensions array) '(2 3 2))
         (= 1 (aref x 0 0 0))
         (eq 'a (aref x 0 0 1))
         (= 2 (aref x 0 1 0))
         (eq 'b (aref x 0 1 1))
         (= 3 (aref x 0 2 0))
         (eq x (aref x 0 2 1))
         (= 4 (aref x 1 0 0))
         (eq 'd (aref x 1 0 1))
         (= 5 (aref x 1 1 0))
         (eq 'e (aref x 1 1 1))
         (= 6 (aref x 1 2 0))
         (eq 'f (aref x 1 2 1))))))
(define-test sacla-must-printer.393 (:tag :sacla)
 (assert-true
  (let* ((array #3A(((1 #1=#:g0) (#2=#:g1 b) (3 #1#)) ((4 d) (5 e) (#2# f))))
         (x (read-from-string (write-to-string array :array t :circle t))))
    (and (equal (array-dimensions array) '(2 3 2))
         (= 1 (aref x 0 0 0))
         (eq (aref x 0 0 1) (aref x 0 2 1))
         (null (symbol-package (aref x 0 0 1)))
         (string= "G0" (symbol-name (aref x 0 0 1)))
         (eq (aref x 0 1 0) (aref x 1 2 0))
         (null (symbol-package (aref x 0 1 0)))
         (string= "G1" (symbol-name (aref x 0 1 0)))
         (eq 'b (aref x 0 1 1))
         (= 3 (aref x 0 2 0))
         (= 4 (aref x 1 0 0))
         (eq 'd (aref x 1 0 1))
         (= 5 (aref x 1 1 0))
         (eq 'e (aref x 1 1 1))
         (eq 'f (aref x 1 2 1))))))
(define-test sacla-must-printer.394 (:tag :sacla)
 (assert-true
  (let* ((array
          #1=#3A(((#1# #2=#:g0) (#3=#:g1 #2#) (#3# #1#))
                 ((#1# #2#) (#2# #3#) (#2# #1#))))
         (x (read-from-string (write-to-string array :array t :circle t))))
    (and (equal (array-dimensions array) '(2 3 2))
         (eq x (aref x 0 0 0))
         (null (symbol-package (aref x 0 0 1)))
         (string= (symbol-name (aref x 0 0 1)) "G0")
         (null (symbol-package (aref x 0 1 0)))
         (string= (symbol-name (aref x 0 1 0)) "G1")
         (eq (aref x 0 1 0) (aref x 0 2 0))
         (eq x (aref x 0 2 1))
         (eq x (aref x 1 0 0))
         (eq (aref x 1 0 1) (aref x 0 0 1))
         (eq (aref x 1 1 0) (aref x 0 0 1))
         (eq (aref x 1 1 1) (aref x 0 1 0))
         (eq (aref x 1 2 0) (aref x 0 0 1))
         (eq (aref x 1 2 1) x)))))
(define-test sacla-must-printer.395 (:tag :sacla)
 (assert-true
  (let* ((array
          #4A((((0 #1=#:g00 2) (#1# 4 #2=#:g01))
               ((#3=#:g02 #2# 8) (9 #4=#:g03 #3#))
               ((#4# 12 #5=#:g04) (#6=#:g05 #6# #5#)))))
         (x (read-from-string (write-to-string array :array t :circle t))))
    (and (equal (array-dimensions array) '(1 3 2 3))
         (= 0 (aref x 0 0 0 0))
         (null (symbol-package (aref x 0 0 0 1)))
         (string= (symbol-name (aref x 0 0 0 1)) "G00")
         (= 2 (aref x 0 0 0 2))
         (eq (aref x 0 0 1 0) (aref x 0 0 0 1))
         (= 4 (aref x 0 0 1 1))
         (null (symbol-package (aref x 0 0 1 2)))
         (string= (symbol-name (aref x 0 0 1 2)) "G01")
         (null (symbol-package (aref x 0 1 0 0)))
         (string= (symbol-name (aref x 0 1 0 0)) "G02")
         (eq (aref x 0 1 0 1) (aref x 0 0 1 2))
         (= 8 (aref x 0 1 0 2))
         (= 9 (aref x 0 1 1 0))
         (null (symbol-package (aref x 0 1 1 1)))
         (string= (symbol-name (aref x 0 1 1 1)) "G03")
         (eq (aref x 0 1 1 2) (aref x 0 1 0 0))
         (eq (aref x 0 2 0 0) (aref x 0 1 1 1))
         (= 12 (aref x 0 2 0 1))
         (null (symbol-package (aref x 0 2 0 2)))
         (string= (symbol-name (aref x 0 2 0 2)) "G04")
         (null (symbol-package (aref x 0 2 1 0)))
         (string= (symbol-name (aref x 0 2 1 0)) "G05")
         (eq (aref x 0 2 1 1) (aref x 0 2 1 0))
         (eq (aref x 0 2 1 2) (aref x 0 2 0 2))))))
(define-test sacla-must-printer.396 (:tag :sacla)
 (assert-true
  (let* ((sequence '#1=(#(0 #2=(#1#) #1# 3) #3=#2A((#1# #2#) (#3# 4))))
         (x (read-from-string (write-to-string sequence :array t :circle t))))
    (and (= 2 (length x))
         (= 4 (length (first x)))
         (= 0 (aref (first x) 0))
         (eq x (first (aref (first x) 1)))
         (eq x (aref (first x) 2))
         (= 3 (aref (first x) 3))
         (equal (array-dimensions (second x)) '(2 2))
         (eq x (aref (second x) 0 0))
         (eq (aref (second x) 0 1) (aref (first x) 1))
         (eq (aref (second x) 1 0) (second x))
         (= 4 (aref (second x) 1 1))))))
(define-test sacla-must-printer.397 (:tag :sacla)
 (assert-true
  (let* ((sequence
          '#1=#(#2=(0 1 . #3=(2)) #(#3# #2# #1#) #3A(((#1# #2# #3#)))))
         (x (read-from-string (write-to-string sequence :array t :circle t))))
    (and (= 3 (length x))
         (= 3 (length (aref x 0)))
         (= 0 (first (aref x 0)))
         (= 1 (second (aref x 0)))
         (= 2 (third (aref x 0)))
         (= 3 (length (aref x 1)))
         (eq (aref (aref x 1) 0) (cddr (aref x 0)))
         (eq (aref (aref x 1) 1) (aref x 0))
         (eq (aref (aref x 1) 2) x)
         (equal (array-dimensions (aref x 2)) '(1 1 3))
         (eq (aref (aref x 2) 0 0 0) x)
         (eq (aref (aref x 2) 0 0 1) (aref x 0))
         (eq (aref (aref x 2) 0 0 2) (cddr (aref x 0)))))))
(define-test sacla-must-printer.398 (:tag :sacla)
 (assert-true
  (let* ((sequence '((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))))
    (string=
     (write-to-string sequence :pretty nil :array t :level 0 :length 10)
     "#"))))
(define-test sacla-must-printer.399 (:tag :sacla)
 (assert-true
  (let* ((sequence '((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))))
    (string=
     (write-to-string sequence :pretty nil :array t :level 1 :length 10)
     "(# # #)"))))
(define-test sacla-must-printer.400 (:tag :sacla)
 (assert-true
  (let* ((sequence '((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))))
    (string=
     (write-to-string sequence :pretty nil :array t :level 2 :length 10)
     "((1 2 3) #(4 5 6) #2A(# #))"))))
(define-test sacla-must-printer.401 (:tag :sacla)
 (assert-true
  (let* ((sequence '((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))))
    (string=
     (write-to-string sequence :pretty nil :array t :level 3 :length 10)
     "((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))"))))
(define-test sacla-must-printer.402 (:tag :sacla)
 (assert-true
  (let* ((sequence '((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))))
    (string= (write-to-string sequence :pretty nil :array t :level 3 :length 1)
             "((1 ...) ...)"))))
(define-test sacla-must-printer.403 (:tag :sacla)
 (assert-true
  (let* ((sequence '((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))))
    (string= (write-to-string sequence :pretty nil :array t :level 3 :length 2)
             "((1 2 ...) #(4 5 ...) ...)"))))
(define-test sacla-must-printer.404 (:tag :sacla)
 (assert-true
  (let* ((sequence '((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))))
    (string= (write-to-string sequence :pretty nil :array t :level 3 :length 3)
             "((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))"))))
(define-test sacla-must-printer.405 (:tag :sacla)
 (assert-true
  (let* ((sequence '((1 2 3) #(4 5 6) #2A((7 8 9 10) (11 12 13 14)))))
    (string= (write-to-string sequence :pretty nil :array t :level 3 :length 3)
             "((1 2 3) #(4 5 6) #2A((7 8 9 ...) (11 12 13 ...)))"))))
(define-test sacla-must-printer.406 (:tag :sacla)
 (assert-true
  (let* ((sequence '#((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))))
    (string=
     (write-to-string sequence :pretty nil :array t :level 0 :length 10)
     "#"))))
(define-test sacla-must-printer.407 (:tag :sacla)
 (assert-true
  (let* ((sequence '#((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))))
    (string=
     (write-to-string sequence :pretty nil :array t :level 1 :length 10)
     "#(# # #)"))))
(define-test sacla-must-printer.408 (:tag :sacla)
 (assert-true
  (let* ((sequence '#((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))))
    (string=
     (write-to-string sequence :pretty nil :array t :level 2 :length 10)
     "#((1 2 3) #(4 5 6) #2A(# #))"))))
(define-test sacla-must-printer.409 (:tag :sacla)
 (assert-true
  (let* ((sequence '#((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))))
    (string=
     (write-to-string sequence :pretty nil :array t :level 3 :length 10)
     "#((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))"))))
(define-test sacla-must-printer.410 (:tag :sacla)
 (assert-true
  (let* ((sequence '#((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))))
    (string= (write-to-string sequence :pretty nil :array t :level 3 :length 1)
             "#((1 ...) ...)"))))
(define-test sacla-must-printer.411 (:tag :sacla)
 (assert-true
  (let* ((sequence '#((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))))
    (string= (write-to-string sequence :pretty nil :array t :level 3 :length 2)
             "#((1 2 ...) #(4 5 ...) ...)"))))
(define-test sacla-must-printer.412 (:tag :sacla)
 (assert-true
  (let* ((sequence '#((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))))
    (string= (write-to-string sequence :pretty nil :array t :level 3 :length 3)
             "#((1 2 3) #(4 5 6) #2A((7 8 9) (10 11 12)))"))))
(define-test sacla-must-printer.413 (:tag :sacla)
 (assert-true
  (let* ((sequence '#((1 2 3) #(4 5 6) #2A((7 8 9 10) (11 12 13 14)))))
    (string= (write-to-string sequence :pretty nil :array t :level 3 :length 3)
             "#((1 2 3) #(4 5 6) #2A((7 8 9 ...) (11 12 13 ...)))"))))
(define-test sacla-must-printer.414 (:tag :sacla)
 (assert-true
  (let* ((array '#2A(((10) #(100)) ((0 1 2) #2A((3) (4) (5) (6) (7))))))
    (string= (write-to-string array :pretty nil :array t :level 0 :length 0)
             "#"))))
(define-test sacla-must-printer.415 (:tag :sacla)
 (assert-true
  (let* ((array '#2A(((10) #(100)) ((0 1 2) #2A((3) (4) (5) (6) (7))))))
    (string= (write-to-string array :pretty nil :array t :level 1 :length 0)
             "#2A(...)"))))
(define-test sacla-must-printer.416 (:tag :sacla)
 (assert-true
  (let* ((array '#2A(((10) #(100)) ((0 1 2) #2A((3) (4) (5) (6) (7))))))
    (string= (write-to-string array :pretty nil :array t :level 1 :length 1)
             "#2A(# ...)"))))
(define-test sacla-must-printer.417 (:tag :sacla)
 (assert-true
  (let* ((array '#2A(((10) #(100)) ((0 1 2) #2A((3) (4) (5) (6) (7))))))
    (string= (write-to-string array :pretty nil :array t :level 2 :length 1)
             "#2A((# ...) ...)"))))
(define-test sacla-must-printer.418 (:tag :sacla)
 (assert-true
  (let* ((array '#2A(((10) #(100)) ((0 1 2) #2A((3) (4) (5) (6) (7))))))
    (string= (write-to-string array :pretty nil :array t :level 2 :length 2)
             "#2A((# #) (# #))"))))
(define-test sacla-must-printer.419 (:tag :sacla)
 (assert-true
  (let* ((array '#2A(((10) #(100)) ((0 1 2) #2A((3) (4) (5) (6) (7))))))
    (string= (write-to-string array :pretty nil :array t :level 3 :length 1)
             "#2A(((10) ...) ...)"))))
(define-test sacla-must-printer.420 (:tag :sacla)
 (assert-true
  (let* ((array '#2A(((10) #(100)) ((0 1 2) #2A((3) (4) (5) (6) (7))))))
    (string= (write-to-string array :pretty nil :array t :level 3 :length 2)
             "#2A(((10) #(100)) ((0 1 ...) #2A(# # ...)))"))))
(define-test sacla-must-printer.421 (:tag :sacla)
 (assert-true
  (let* ((array '#2A(((10) #(100)) ((0 1 2) #2A((3) (4) (5) (6) (7))))))
    (string= (write-to-string array :pretty nil :array t :level 4 :length 2)
             "#2A(((10) #(100)) ((0 1 ...) #2A((3) (4) ...)))"))))
(define-test sacla-must-printer.422 (:tag :sacla)
 (assert-true
  (let* ((array '#2A(((10) #(100)) ((0 1 2) #2A((3) (4) (5) (6) (7))))))
    (string= (write-to-string array :pretty nil :array t :level 4 :length 3)
             "#2A(((10) #(100)) ((0 1 2) #2A((3) (4) (5) ...)))"))))
(define-test sacla-must-printer.423 (:tag :sacla)
 (assert-true
  (let* ((array '#2A(((10) #(100)) ((0 1 2) #2A((3) (4) (5) (6) (7))))))
    (string= (write-to-string array :pretty nil :array t :level 4 :length 5)
             "#2A(((10) #(100)) ((0 1 2) #2A((3) (4) (5) (6) (7))))"))))
(define-test sacla-must-printer.424 (:tag :sacla)
 (assert-true
  (let* ((array
          '#2A(((10) #((100)))
               ((0 (1) ((2))) #2A((3) ((4)) (((5))) ((6)) (7))))))
    (string= (write-to-string array :pretty nil :array t :level 3 :length 5)
             "#2A(((10) #(#)) ((0 # #) #2A(# # # # #)))"))))
(define-test sacla-must-printer.425 (:tag :sacla)
 (assert-true
  (let* ((array
          '#2A(((10) #((100)))
               ((0 (1) ((2))) #2A((3) ((4)) (((5))) ((6)) (7))))))
    (string= (write-to-string array :pretty nil :array t :level 4 :length 5)
             "#2A(((10) #((100))) ((0 (1) (#)) #2A((3) (#) (#) (#) (7))))"))))
(define-test sacla-must-printer.426 (:tag :sacla)
 (assert-true
  (let* ((array
          '#2A(((10) #((100)))
               ((0 (1) ((2))) #2A((3) ((4)) (((5))) ((6)) (7))))))
    (string= (write-to-string array :pretty nil :array t :level 5 :length 5)
             "#2A(((10) #((100))) ((0 (1) ((2))) #2A((3) ((4)) ((#)) ((6)) (7))))"))))
(define-test sacla-must-printer.427 (:tag :sacla)
 (assert-true
  (let* ((array
          '#2A(((10) #((100)))
               ((0 (1) ((2))) #2A((3) ((4)) (((5))) ((6)) (7))))))
    (string= (write-to-string array :pretty nil :array t :level 6 :length 4)
             "#2A(((10) #((100))) ((0 (1) ((2))) #2A((3) ((4)) (((5))) ((6)) ...)))"))))
(define-test sacla-must-printer.428 (:tag :sacla)
 (assert-true
  (equal
   (read-from-string
    (write-to-string '(0 1 2) :pretty nil :readably t :level 0 :length 0))
   '(0 1 2))))
(define-test sacla-must-printer.429 (:tag :sacla)
 (assert-true
  (equalp
   (read-from-string
    (write-to-string #(0 1 2) :pretty nil :readably t :level 0 :length 0))
   #(0 1 2))))
(define-test sacla-must-printer.430 (:tag :sacla)
 (assert-true
  (equalp
   (read-from-string
    (write-to-string #2A((0) (1) (2))
                     :pretty nil
                     :readably t
                     :level 0
                     :length 0))
   #2A((0) (1) (2)))))
(define-test sacla-must-printer.431 (:tag :sacla)
 (assert-true
  (string= "LENGTH" (write-to-string 'length :escape nil :level 0))))
(define-test sacla-must-printer.432 (:tag :sacla)
 (assert-true
  (string= "LENGTH" (write-to-string 'length :escape nil :length 2))))
(define-test sacla-must-printer.433 (:tag :sacla)
 (assert-true
  (string= "LENGTH" (write-to-string 'length :escape nil :level 0 :length 0))))
(define-test sacla-must-printer.434 (:tag :sacla)
 (assert-true
  (string= "abcdefg" (write-to-string "abcdefg" :escape nil :level 0))))
(define-test sacla-must-printer.435 (:tag :sacla)
 (assert-true
  (string= "abcdefg" (write-to-string "abcdefg" :escape nil :length 2))))
(define-test sacla-must-printer.436 (:tag :sacla)
 (assert-true
  (string= "abcdefg"
           (write-to-string "abcdefg" :escape nil :level 0 :length 0))))
(define-test sacla-must-printer.437 (:tag :sacla)
 (assert-true (string= "#*0101" (write-to-string #*0101 :array t :level 0))))
(define-test sacla-must-printer.438 (:tag :sacla)
 (assert-true (string= "#*0101" (write-to-string #*0101 :array t :length 2))))
(define-test sacla-must-printer.439 (:tag :sacla)
 (assert-true
  (string= "#*0101" (write-to-string #*0101 :array t :level 0 :length 0))))

