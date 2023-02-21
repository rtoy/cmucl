(defpackage :printer-tests
  (:use :cl :lisp-unit))

(in-package "PRINTER-TESTS")

(define-test format.float.1
  (assert-equal ".0000"
		(format nil "~5F" 1d-10))
  (assert-equal "0.000"
		(format nil "~,3F" 0.000001)))

(define-test format.float.2
  (assert-equal "  0.990E+00" (format nil "~11,3,2,0,'*,,'EE" .99))
  (assert-equal "  0.999E+00" (format nil "~11,3,2,0,'*,,'EE" .999))
  (assert-equal "  0.100E+01" (format nil "~11,3,2,0,'*,,'EE" .9999))
  (assert-equal "  0.999E-04" (format nil "~11,3,2,0,'*,,'EE" .0000999))
  (assert-equal "  0.100E-03" (format nil "~11,3,2,0,'*,,'EE" .00009999))
  (assert-equal "  9.999E-05" (format nil "~11,3,2,,'*,,'EE" .00009999))
  (assert-equal "  1.000E-04" (format nil "~11,3,2,,'*,,'EE" .000099999)))

(define-test format.float.3
  (assert-equal ".00123d+6" (format nil "~9,,,-2E" 1.2345689d3))
  (assert-equal "-.0012d+6" (format nil "~9,,,-2E" -1.2345689d3))
  (assert-equal ".00123d+0" (format nil "~9,,,-2E" 1.2345689d-3))
  (assert-equal "-.0012d+0" (format nil "~9,,,-2E" -1.2345689d-3)))

(define-test format.float.4
  (assert-equal "0.314e-01" (format nil "~9,3,2,0,'%G" 0.0314159))
  (assert-equal "+.003e+03" (format nil "~9,3,2,-2,'%@e" 3.14159))
  (assert-equal " 31.42" (format nil "~6,2,1,'*F" 3.14159))
  (assert-equal " 3141590." (format nil "~9,0,6f" 3.14159))
    
  (assert-equal ".00000003d+8" (format nil "~9,4,,-7E" pi))
  (assert-equal ".000003d+6" (format nil "~9,4,,-5E" pi))
  (assert-equal "3141600.d-6" (format nil "~5,4,,7E" pi))
  (assert-equal "  314.16d-2" (format nil "~11,4,,3E" pi))
  (assert-equal "  31416.d-4" (format nil "~11,4,,5E" pi))
  (assert-equal "  0.3142d+1" (format nil "~11,4,,0E" pi))
  (assert-equal ".03142d+2" (format nil "~9,,,-1E" pi))
  (assert-equal "0.003141592653589793d+3" (format nil "~,,,-2E" pi))
  (assert-equal "31.41592653589793d-1" (format nil "~,,,2E" pi))
  (assert-equal "3.141592653589793d+0" (format nil "~E" pi))
  (assert-equal ".03142d+2" (format nil "~9,5,,-1E" pi))
  (assert-equal " 0.03142d+2" (format nil "~11,5,,-1E" pi))
  (assert-equal "3.141592653589793    " (format nil "~G" pi))
  (assert-equal "3.1416    " (format nil "~9,5G" pi))
  (assert-equal "| 3141593.d-06|" (format nil "|~13,6,2,7E|" pi))
  (assert-equal "0.314d+01" (format nil "~9,3,2,0,'%E" pi))
  (assert-equal " 3141593." (format nil "~9,0,6f" pi))
  (assert-equal " 31.42" (format nil "~6,2,1,'*F" pi))
  (assert-equal "******" (format nil "~6,2,1,'*F" (* 100 pi)))
  (assert-equal "+.003d+03" (format nil "~9,3,2,-2,'%@E" pi))
  (assert-equal "+0.003d+03" (format nil "~10,3,2,-2,'%@E" pi))
  (assert-equal "=====+0.003d+03" (format nil "~15,3,2,-2,'%,'=@E" pi))
  (assert-equal "0.003d+03" (format nil "~9,3,2,-2,'%E" pi))
  (assert-equal "%%%%%%%%" (format nil "~8,3,2,-2,'%@E" pi))
    
  (assert-equal "1.    " (format nil "~g" 1e0))
    
  (assert-equal "0.0e+0" (format nil "~e" 0))
  (assert-equal "0.0d+0" (format nil "~e" 0d0))
  (assert-equal "0.0d+0000" (format nil "~9,,4e" 0d0))
  (assert-equal "1.2345678901234567d+4" (format nil "~E" 1.234567890123456789d4))
    
  (assert-equal "1.32922799578492d+36" (format nil "~20E" (expt 2d0 120)))
  (assert-equal "       1.32922800d+36" (format nil "~21,8E" (expt 2d0 120)))
    
  (assert-equal ".0012345679" (format nil "~11f" 1.23456789123456789d-3)))

(define-test format.float.5
  ;; From CLHS 22.3.11
  (flet ((test-f (x)
	   (format nil "~6,2F|~6,2,1,'*F|~6,2,,'?F|~6F|~,2F|~F"
		   x x x x x x)))
    (assert-equal "  3.14| 31.42|  3.14|3.1416|3.14|3.14159" (test-f 3.14159))
    (assert-equal " -3.14|-31.42| -3.14|-3.142|-3.14|-3.14159" (test-f -3.14159))
    (assert-equal "100.00|******|100.00| 100.0|100.00|100.0" (test-f 100.0))
    (assert-equal "1234.00|******|??????|1234.0|1234.00|1234.0" (test-f 1234.0))
    (assert-equal "  0.01|  0.06|  0.01| 0.006|0.01|0.006" (test-f 0.006))))

(define-test format.float.6
  (flet ((test-e (x)
	   (format nil
		   "~9,2,1,,'*E|~10,3,2,2,'?,,'$E|~
                ~9,3,2,-2,'%@E|~9,2E"
		   x x x x)))
    
    (assert-equal "  3.14e+0| 31.42$-01|+.003e+03|  3.14e+0" (test-e 3.14159))
    (assert-equal " -3.14e+0|-31.42$-01|-.003e+03| -3.14e+0" (test-e -3.14159))
    (assert-equal "  1.10e+3| 11.00$+02|+.001e+06|  1.10e+3" (test-e 1100.0))
    (assert-equal "  1.10d+3| 11.00$+02|+.001d+06|  1.10d+3" (test-e 1100.0d0))
    (assert-equal "*********| 11.00$+12|+.001e+16| 1.10e+13" (test-e 1.1e13))
    (assert-equal "*********|??????????|%%%%%%%%%|1.10d+120" (test-e 1.1d120))))

(define-test format.float.7
  (flet ((test-scale (k)
	   (format nil "~&Scale factor ~2D: |~13,6,2,VE|"
		   (- k 5) (- k 5) 3.14159)))
    
    (assert-equal "Scale factor -5: | 0.000003e+06|" (test-scale 0))
    (assert-equal "Scale factor -4: | 0.000031e+05|" (test-scale 1))
    (assert-equal "Scale factor -3: | 0.000314e+04|" (test-scale 2))
    (assert-equal "Scale factor -2: | 0.003142e+03|" (test-scale 3))
    (assert-equal "Scale factor -1: | 0.031416e+02|" (test-scale 4))
    (assert-equal "Scale factor  0: | 0.314159e+01|" (test-scale 5))
    (assert-equal "Scale factor  1: | 3.141590e+00|" (test-scale 6))
    (assert-equal "Scale factor  2: | 31.41590e-01|" (test-scale 7))
    (assert-equal "Scale factor  3: | 314.1590e-02|" (test-scale 8))
    (assert-equal "Scale factor  4: | 3141.590e-03|" (test-scale 9))
    (assert-equal "Scale factor  5: | 31415.90e-04|" (test-scale 10))
    (assert-equal "Scale factor  6: | 314159.0e-05|" (test-scale 11))
    (assert-equal "Scale factor  7: | 3141590.e-06|" (test-scale 12))))

(define-test sub-output-integer.1
    (assert-prints "-536870912" (princ most-negative-fixnum)))

;;; Simple LOOP requires only compound forms. Hence NIL is not
;;; permitted. Some FORMAT directives (like newline) return NIL
;;; as the form when they have nothing to add to the body.
;;; Normally this is fine since BLOCK accepts NIL as a form. On
;;; the other hand, when the newline directive is inside of an
;;; iteration directive this will produce something like
;;; (LOOP (fu) nil (bar)) which is not acceptable. To verify
;;; that this is not happening we make sure we are not getting
;;; (BLOCK NIL NIL) since this is easier to test for.
(define-test format-no-nil-form.1
    (assert-equal '(block nil) (third (second (macroexpand-1 '(formatter "~
"))))))
