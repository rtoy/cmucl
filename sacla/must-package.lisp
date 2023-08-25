(in-package #:sacla-lisp-unit)
(define-test sacla-must-package.1 (:tag :sacla)
 (assert-true (listp (list-all-packages))))
(define-test sacla-must-package.2 (:tag :sacla)
 (assert-true
  (find "COMMON-LISP"
        (mapcar #'package-name (list-all-packages))
        :test #'string=)))
(define-test sacla-must-package.3 (:tag :sacla)
 (assert-true
  (find "COMMON-LISP-USER"
        (mapcar #'package-name (list-all-packages))
        :test #'string=)))
(define-test sacla-must-package.4 (:tag :sacla)
 (assert-true
  (find "KEYWORD" (mapcar #'package-name (list-all-packages)) :test #'string=)))
(define-test sacla-must-package.5 (:tag :sacla)
 (assert-true (every #'packagep (list-all-packages))))
(define-test sacla-must-package.6 (:tag :sacla)
 (assert-true (packagep (find-package "COMMON-LISP"))))
(define-test sacla-must-package.7 (:tag :sacla)
 (assert-true (packagep (find-package "CL"))))
(define-test sacla-must-package.8 (:tag :sacla)
 (assert-true (packagep (find-package "COMMON-LISP-USER"))))
(define-test sacla-must-package.9 (:tag :sacla)
 (assert-true (packagep (find-package "CL-USER"))))
(define-test sacla-must-package.10 (:tag :sacla)
 (assert-true (packagep (find-package "KEYWORD"))))
(define-test sacla-must-package.11 (:tag :sacla)
 (assert-true
  (let ((cl (find-package "COMMON-LISP")))
    (eq cl (find-package cl)))))
(define-test sacla-must-package.12 (:tag :sacla)
 (assert-true (eq (find-package "CL") (find-package "COMMON-LISP"))))
(define-test sacla-must-package.13 (:tag :sacla)
 (assert-true (eq (find-package 'cl) (find-package "COMMON-LISP"))))
(define-test sacla-must-package.14 (:tag :sacla)
 (assert-true (eq (find-package 'cl) (find-package 'common-lisp))))
(define-test sacla-must-package.15 (:tag :sacla)
 (assert-true
  (let ((name "NO-SUCH-PACKAGE"))
    (when (find-package name)
      (delete-package name))
    (not (find-package name)))))
(define-test sacla-must-package.16 (:tag :sacla)
 (assert-true (= (length (multiple-value-list (find-package "CL"))) 1)))
(define-test sacla-must-package.17 (:tag :sacla)
 (assert-true
  (= (length (multiple-value-list (find-package "NO-SUCH-PACKAGE"))) 1)))
(define-test sacla-must-package.18 (:tag :sacla)
 (assert-true
  (packagep (find-package (find-package (find-package "KEYWORD"))))))
(define-test sacla-must-package.19 (:tag :sacla)
 (assert-true
  (every (complement #'packagep)
         '(nil a b "CL" "KEYWORD" (a) cl common-lisp-user))))
(define-test sacla-must-package.20 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "a")
      (delete-package "a"))
    (and (packagep (make-package #\a)) (delete-package "a")))))
(define-test sacla-must-package.21 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "a")
      (delete-package "a"))
    (and (packagep (make-package '|a|)) (delete-package "a")))))
(define-test sacla-must-package.22 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "a")
      (delete-package "a"))
    (and (packagep (make-package "a")) (delete-package "a")))))
(define-test sacla-must-package.23 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "a")
      (delete-package "a"))
    (and (packagep (make-package "a" :use nil)) (delete-package "a")))))
(define-test sacla-must-package.24 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "a")
      (delete-package "a"))
    (and (packagep (make-package "a" :use '(cl))) (delete-package "a")))))
(define-test sacla-must-package.25 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "a")
      (delete-package "a"))
    (and (packagep (make-package "a" :use '(cl) :nicknames '("b")))
         (delete-package "b")))))
(define-test sacla-must-package.26 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "a")
      (delete-package "a"))
    (and (packagep (make-package "a" :use '(cl) :nicknames '("b" "c")))
         (delete-package "c")))))
(define-test sacla-must-package.27 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "a")
      (delete-package "a"))
    (and (packagep (make-package "a" :use '(cl) :nicknames '(#\b "c")))
         (delete-package "b")))))
(define-test sacla-must-package.28 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "a")
      (delete-package "a"))
    (and (packagep (make-package "a" :use '(cl) :nicknames '(|b| "c")))
         (delete-package "b")))))
(define-test sacla-must-package.29 (:tag :sacla)
 (assert-true
  (handler-case
      (progn
        (when (find-package "a")
          (delete-package "a"))
        (when (find-package "b")
          (delete-package "b"))
        (and (packagep (make-package "b" :use '(cl)))
             (packagep (make-package "a" :use '(cl) :nicknames '(|b| "c")))))
    (error nil t)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-package.30 (:tag :sacla)
 (assert-true
  (handler-case
      (progn
        (when (find-package "a")
          (delete-package "a"))
        (when (find-package "b")
          (delete-package "b"))
        (and (packagep (make-package "a" :use '(cl)))
             (packagep (make-package "a" :use '(cl) :nicknames '(|b| "c")))))
    (error nil t)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-package.31 (:tag :sacla)
 (assert-true
  (handler-case
      (progn
        (when (find-package "a")
          (delete-package "a"))
        (when (find-package "d")
          (delete-package "b"))
        (and (packagep (make-package "a" :use '(cl) :nicknames '("b" "c")))
             (packagep (make-package "d" :use '(cl) :nicknames '("c")))))
    (error nil t)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-package.32 (:tag :sacla)
 (assert-true
  (handler-case
      (progn
        (when (find-package "TB-FOO")
          (delete-package "TB-FOO"))
        (when (find-package "TB-BAR-TO-USE")
          (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
          (delete-package "TB-BAR-TO-USE"))
        (and (packagep (make-package "TB-BAR-TO-USE" :use nil))
             (export (intern "CAR" 'tb-bar-to-use) 'tb-bar-to-use)
             (make-package "TB-FOO" :use '(cl "TB-BAR-TO-USE"))))
    (error nil t)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-package.33 (:tag :sacla)
 (assert-true (string= (package-name "COMMON-LISP") "COMMON-LISP")))
(define-test sacla-must-package.34 (:tag :sacla)
 (assert-true (string= (package-name 'common-lisp) "COMMON-LISP")))
(define-test sacla-must-package.35 (:tag :sacla)
 (assert-true
  (string= (package-name (find-package 'common-lisp)) "COMMON-LISP")))
(define-test sacla-must-package.36 (:tag :sacla)
 (assert-true (string= (package-name "CL") "COMMON-LISP")))
(define-test sacla-must-package.37 (:tag :sacla)
 (assert-true (string= (package-name 'cl) "COMMON-LISP")))
(define-test sacla-must-package.38 (:tag :sacla)
 (assert-true (string= (package-name (find-package 'cl)) "COMMON-LISP")))
(define-test sacla-must-package.39 (:tag :sacla)
 (assert-true
  (let ((designator-list
         (list 'cl
               'common-lisp
               "CL"
               "COMMON-LISP"
               (find-package 'cl)
               'cl-user
               'common-lisp-user
               "CL-USER"
               "COMMON-LISP-USER"
               (find-package 'cl-user)
               'keyword
               "KEYWORD"
               (find-package 'keyword))))
    (every #'stringp (mapcar #'package-name designator-list)))))
(define-test sacla-must-package.40 (:tag :sacla)
 (assert-true (every #'stringp (mapcar #'package-name (list-all-packages)))))
(define-test sacla-must-package.41 (:tag :sacla)
 (assert-true
  (let* ((name "TB-FOO")
         (package (or (find-package name) (make-package name :use nil))))
    (and (delete-package name)
         (not (find-package name))
         (null (package-name package))))))
(define-test sacla-must-package.42 (:tag :sacla)
 (assert-true (member "CL" (package-nicknames "COMMON-LISP") :test #'string=)))
(define-test sacla-must-package.43 (:tag :sacla)
 (assert-true (member "CL" (package-nicknames 'common-lisp) :test #'string=)))
(define-test sacla-must-package.44 (:tag :sacla)
 (assert-true
  (member "CL"
          (package-nicknames (find-package 'common-lisp))
          :test #'string=)))
(define-test sacla-must-package.45 (:tag :sacla)
 (assert-true (member "CL" (package-nicknames "CL") :test #'string=)))
(define-test sacla-must-package.46 (:tag :sacla)
 (assert-true (member "CL" (package-nicknames 'cl) :test #'string=)))
(define-test sacla-must-package.47 (:tag :sacla)
 (assert-true
  (member "CL" (package-nicknames (find-package 'cl)) :test #'string=)))
(define-test sacla-must-package.48 (:tag :sacla)
 (assert-true
  (let ((name 'test-foo)
        (nicknames '(test-foo-nickname1 test-foo-nickname2 test-foo-nickname3)))
    (dolist (name (cons name nicknames))
      (when (find-package name)
        (delete-package name)))
    (every #'stringp
           (package-nicknames (make-package name :nicknames nicknames))))))
(define-test sacla-must-package.49 (:tag :sacla)
 (assert-true
  (every #'stringp
         (mapcan #'(lambda (package) (copy-list (package-nicknames package)))
                 (list-all-packages)))))
(define-test sacla-must-package.50 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package 'test-foo)
      (delete-package 'test-foo))
    (null
     (set-difference
      (package-nicknames
       (make-package 'test-foo :nicknames '("TB-FOO" "test-foo")))
      '("TB-FOO" "test-foo")
      :test #'string=)))))
(define-test sacla-must-package.51 (:tag :sacla)
 (assert-true
  (let ((designator-list
         (list 'cl
               'common-lisp
               "CL"
               "COMMON-LISP"
               (find-package 'cl)
               'cl-user
               'common-lisp-user
               "CL-USER"
               "COMMON-LISP-USER"
               (find-package 'cl-user)
               'keyword
               "KEYWORD"
               (find-package 'keyword))))
    (every #'stringp
           (mapcan
            #'(lambda (designator) (copy-list (package-nicknames designator)))
            designator-list)))))
(define-test sacla-must-package.52 (:tag :sacla)
 (assert-true
  (every #'listp (mapcar #'package-shadowing-symbols (list-all-packages)))))
(define-test sacla-must-package.53 (:tag :sacla)
 (assert-true
  (every #'symbolp
         (mapcan
          #'(lambda (package) (copy-list (package-shadowing-symbols package)))
          (list-all-packages)))))
(define-test sacla-must-package.54 (:tag :sacla)
 (assert-true (listp (package-shadowing-symbols 'cl))))
(define-test sacla-must-package.55 (:tag :sacla)
 (assert-true (listp (package-shadowing-symbols "CL-USER"))))
(define-test sacla-must-package.56 (:tag :sacla)
 (assert-true (listp (package-shadowing-symbols "COMMON-LISP"))))
(define-test sacla-must-package.57 (:tag :sacla)
 (assert-true (listp (package-shadowing-symbols (find-package 'keyword)))))
(define-test sacla-must-package.58 (:tag :sacla)
 (assert-true
  (let ((designator-list
         (list 'cl
               'common-lisp
               "CL"
               "COMMON-LISP"
               (find-package 'cl)
               'cl-user
               'common-lisp-user
               "CL-USER"
               "COMMON-LISP-USER"
               (find-package 'cl-user)
               'keyword
               "KEYWORD"
               (find-package 'keyword))))
    (every #'symbolp
           (mapcan
            #'(lambda (designator)
                (copy-list (package-shadowing-symbols designator)))
            designator-list)))))
(define-test sacla-must-package.59 (:tag :sacla)
 (assert-true (every #'listp (mapcar #'package-use-list (list-all-packages)))))
(define-test sacla-must-package.60 (:tag :sacla)
 (assert-true
  (every #'packagep
         (mapcan #'(lambda (package) (copy-list (package-use-list package)))
                 (list-all-packages)))))
(define-test sacla-must-package.61 (:tag :sacla)
 (assert-true (listp (package-use-list 'cl))))
(define-test sacla-must-package.62 (:tag :sacla)
 (assert-true (listp (package-use-list "CL-USER"))))
(define-test sacla-must-package.63 (:tag :sacla)
 (assert-true (listp (package-use-list "COMMON-LISP"))))
(define-test sacla-must-package.64 (:tag :sacla)
 (assert-true (listp (package-use-list (find-package 'keyword)))))
(define-test sacla-must-package.65 (:tag :sacla)
 (assert-true
  (let ((designator-list
         (list 'cl
               'common-lisp
               "CL"
               "COMMON-LISP"
               (find-package 'cl)
               'cl-user
               'common-lisp-user
               "CL-USER"
               "COMMON-LISP-USER"
               (find-package 'cl-user)
               'keyword
               "KEYWORD"
               (find-package 'keyword))))
    (every #'packagep
           (mapcan
            #'(lambda (designator) (copy-list (package-use-list designator)))
            designator-list)))))
(define-test sacla-must-package.66 (:tag :sacla)
 (assert-true
  (every #'listp (mapcar #'package-used-by-list (list-all-packages)))))
(define-test sacla-must-package.67 (:tag :sacla)
 (assert-true
  (every #'packagep
         (mapcan
          #'(lambda (package) (copy-list (package-used-by-list package)))
          (list-all-packages)))))
(define-test sacla-must-package.68 (:tag :sacla)
 (assert-true (listp (package-used-by-list 'cl))))
(define-test sacla-must-package.69 (:tag :sacla)
 (assert-true (listp (package-used-by-list "CL-USER"))))
(define-test sacla-must-package.70 (:tag :sacla)
 (assert-true (listp (package-used-by-list "COMMON-LISP"))))
(define-test sacla-must-package.71 (:tag :sacla)
 (assert-true (listp (package-used-by-list (find-package 'keyword)))))
(define-test sacla-must-package.72 (:tag :sacla)
 (assert-true
  (let ((designator-list
         (list 'cl
               'common-lisp
               "CL"
               "COMMON-LISP"
               (find-package 'cl)
               'cl-user
               'common-lisp-user
               "CL-USER"
               "COMMON-LISP-USER"
               (find-package 'cl-user)
               'keyword
               "KEYWORD"
               (find-package 'keyword))))
    (every #'packagep
           (mapcan
            #'(lambda (designator)
                (copy-list (package-used-by-list designator)))
            designator-list)))))
(define-test sacla-must-package.73 (:tag :sacla)
 (assert-true
  (progn
    (mapcar
     #'(lambda (package)
         (when (find-package package)
           (delete-package package)))
     '("TB-FOO" "TB-FOO-RENAMED"))
    (let* ((package (make-package "TB-FOO" :use nil)))
      (and (eq (rename-package "TB-FOO" "TB-FOO-RENAMED") package)
           (eq (find-package "TB-FOO-RENAMED") package))))))
(define-test sacla-must-package.74 (:tag :sacla)
 (assert-true
  (progn
    (mapcar
     #'(lambda (package)
         (when (find-package package)
           (delete-package package)))
     '("TB-FOO-0" "TB-FOO-1" "TB-FOO-2" "TB-FOO-3" "TB-FOO-4"))
    (let* ((package (make-package "TB-FOO-0" :use nil)))
      (and (eq (rename-package "TB-FOO-0" "TB-FOO-1") package)
           (eq (rename-package "TB-FOO-1" "TB-FOO-2") package)
           (eq (rename-package "TB-FOO-2" "TB-FOO-3") package)
           (eq (rename-package "TB-FOO-3" "TB-FOO-4") package)
           (eq (find-package "TB-FOO-4") package))))))
(define-test sacla-must-package.75 (:tag :sacla)
 (assert-true
  (progn
    (mapcar
     #'(lambda (package)
         (when (find-package package)
           (delete-package package)))
     '("TB-FOO-0" "TB-FOO-1" "TB-FOO-2" "TB-FOO-3" "TB-FOO-4"))
    (let* ((package (make-package "TB-FOO-0" :use nil)))
      (and (eq (rename-package (find-package "TB-FOO-0") "TB-FOO-1") package)
           (eq (rename-package (find-package "TB-FOO-1") "TB-FOO-2") package)
           (eq (rename-package (find-package "TB-FOO-2") "TB-FOO-3") package)
           (eq (rename-package (find-package "TB-FOO-3") "TB-FOO-4") package)
           (eq (find-package "TB-FOO-4") package))))))
(define-test sacla-must-package.76 (:tag :sacla)
 (assert-true
  (progn
    (mapcar
     #'(lambda (package)
         (when (find-package package)
           (delete-package package)))
     '(#\a #\b))
    (let ((package (make-package #\a :use nil)))
      (and (eq (rename-package #\a #\b) package)
           (eq (find-package #\b) package)
           (string= (package-name package) #\b))))))
(define-test sacla-must-package.77 (:tag :sacla)
 (assert-true
  (let ((name-list (list #\a 'b "TB-FOO-0" "TB-FOO-1" 'test-foo-2)))
    (mapcar
     #'(lambda (package)
         (when (find-package package)
           (delete-package package)))
     name-list)
    (let* ((old (pop name-list)) (package (make-package old :use nil)))
      (dolist (new name-list t)
        (unless (eq (rename-package old new) package)
          (return nil))
        (setq old new))))))
(define-test sacla-must-package.78 (:tag :sacla)
 (assert-true
  (progn
    (mapcar
     #'(lambda (package)
         (when (find-package package)
           (delete-package package)))
     '("TB-FOO" "TB-FOO-RENAMED" "TB-FOO-NICKNAME-0" "TB-FOO-NICKNAME-1"))
    (let* ((package
            (make-package "TB-FOO"
                          :use nil
                          :nicknames '("TB-FOO-NICKNAME-0"
                                       "TB-FOO-NICKNAME-1"))))
      (and (eq (rename-package "TB-FOO" "TB-FOO-RENAMED") package)
           (eq (find-package "TB-FOO-RENAMED") package)
           (null
            (set-difference (package-nicknames "TB-FOO-RENAMED")
                            '("TB-FOO-NICKNAME-0" "TB-FOO-NICKNAME-1")
                            :test #'string=)))))))
(define-test sacla-must-package.79 (:tag :sacla)
 (assert-true
  (progn
    (mapcar
     #'(lambda (package)
         (when (find-package package)
           (delete-package package)))
     '("TB-FOO-0" "TB-FOO-1" "TB-FOO-2" "TB-FOO-3" "TB-FOO-4" "TB-FOO-5"))
    (let* ((package
            (make-package "TB-FOO-0"
                          :use nil
                          :nicknames '("TB-FOO-1" "TB-FOO-2"))))
      (and
       (eq (rename-package package "TB-FOO-3" '("TB-FOO-4" "TB-FOO-5"))
           package)
       (eq (find-package "TB-FOO-3") package)
       (eq (find-package "TB-FOO-4") package)
       (eq (find-package "TB-FOO-5") package)
       (not (every #'find-package '("TB-FOO-0" "TB-FOO-1" "TB-FOO-2"))))))))
(define-test sacla-must-package.80 (:tag :sacla)
 (assert-true
  (progn
    (mapcar
     #'(lambda (package)
         (when (find-package package)
           (delete-package package)))
     '("TB-FOO-0" "TB-FOO-1" "TB-FOO-2"))
    (let* ((package
            (make-package "TB-FOO-0" :use nil :nicknames '("TB-FOO-1"))))
      (eq (rename-package package "TB-FOO-1" '("TB-FOO-2")) package)))))
(define-test sacla-must-package.81 (:tag :sacla)
 (assert-true
  (progn
    (mapcar
     #'(lambda (package)
         (when (find-package package)
           (delete-package package)))
     '("TB-FOO-0" "TB-FOO-1" "TB-FOO-2" "TB-FOO-3" "TB-FOO-4" "TB-FOO-5"))
    (let* ((package
            (make-package "TB-FOO-0" :use nil :nicknames '("TB-FOO-1"))))
      (and (eq (rename-package package "TB-FOO-1" '("TB-FOO-2")) package)
           (eq (rename-package package "TB-FOO-2" '("TB-FOO-3")) package)
           (eq (rename-package package "TB-FOO-3" '("TB-FOO-4")) package)
           (eq (rename-package package "TB-FOO-4" '("TB-FOO-5")) package)
           (eq (rename-package package "TB-FOO-5" '("TB-FOO-0")) package)
           (eq (find-package 'test-foo-5) (find-package 'test-foo-0)))))))
(define-test sacla-must-package.82 (:tag :sacla)
 (assert-true
  (progn
    (mapcar
     #'(lambda (package)
         (when (find-package package)
           (delete-package package)))
     '("TB-FOO-0" "TB-FOO-1" "TB-FOO-2"))
    (let* ((package
            (make-package "TB-FOO-0"
                          :use nil
                          :nicknames '("TB-FOO-1" "TB-FOO-2"))))
      (and
       (eq (rename-package package "TB-FOO-2" '("TB-FOO-3" "TB-FOO-1"))
           package)
       (string= (package-name package) "TB-FOO-2")
       (null
        (set-difference (package-nicknames package)
                        '("TB-FOO-3" "TB-FOO-1")
                        :test #'string=)))))))
(define-test sacla-must-package.83 (:tag :sacla)
 (assert-true
  (progn
    (mapcar
     #'(lambda (package)
         (when (find-package package)
           (delete-package package)))
     '("TB-FOO-0" "TB-FOO-1" "TB-FOO-2"))
    (let* ((package
            (make-package "TB-FOO-0"
                          :use nil
                          :nicknames '("TB-FOO-1" "TB-FOO-2"))))
      (and (eq (rename-package package "TB-FOO-3") package)
           (string= (package-name package) "TB-FOO-3")
           (null (package-nicknames package)))))))
(define-test sacla-must-package.84 (:tag :sacla)
 (assert-true
  (equal (multiple-value-list (find-symbol "CAR" "CL")) '(car :external))))
(define-test sacla-must-package.85 (:tag :sacla)
 (assert-true
  (equal (multiple-value-list (find-symbol "CDR" "CL")) '(cdr :external))))
(define-test sacla-must-package.86 (:tag :sacla)
 (assert-true
  (equal (multiple-value-list (find-symbol "CDR" 'cl)) '(cdr :external))))
(define-test sacla-must-package.87 (:tag :sacla)
 (assert-true
  (equal (multiple-value-list (find-symbol "CDR" (find-package 'cl)))
         '(cdr :external))))
(define-test sacla-must-package.88 (:tag :sacla)
 (assert-true
  (equal (multiple-value-list (find-symbol "NIL" "CL")) '(nil :external))))
(define-test sacla-must-package.89 (:tag :sacla)
 (assert-true
  (let ((*package* (find-package 'cl)))
    (equal (multiple-value-list (find-symbol "CDR")) '(cdr :external)))))
(define-test sacla-must-package.90 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "A")
      (delete-package "A"))
    (make-package "A" :use nil)
    (equal (multiple-value-list (find-symbol "A" #\A)) '(nil nil)))))
(define-test sacla-must-package.91 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (make-package "TB-FOO" :use nil)
    (equal (multiple-value-list (find-symbol "A" "TB-FOO")) '(nil nil)))))
(define-test sacla-must-package.92 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (make-package "TB-FOO" :use nil)
    (multiple-value-bind (symbol0 status0)
        (intern "A" "TB-FOO")
      (multiple-value-bind (symbol1 status1)
          (find-symbol "A" "TB-FOO")
        (and (eq symbol0 symbol1) (null status0) (eq status1 :internal)))))))
(define-test sacla-must-package.93 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (make-package "TB-FOO" :use '("CL"))
    (equal (multiple-value-list (find-symbol "CAR" "TB-FOO"))
           '(car :inherited)))))
(define-test sacla-must-package.94 (:tag :sacla)
 (assert-true
  (do-external-symbols (symbol "CL" t)
    (multiple-value-bind (symbol-found status)
        (find-symbol (symbol-name symbol) "COMMON-LISP-USER")
      (unless (and (eq symbol symbol-found) (eq status :inherited))
        (error "Symbol ~S is ~S" symbol-found status))))))
(define-test sacla-must-package.95 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use '("COMMON-LISP"))))
      (and
       (equal (multiple-value-list (find-symbol "APPEND"))
              '(append :inherited))
       (equal (multiple-value-list (find-symbol "FIND")) '(find :inherited))
       (equal (multiple-value-list (find-symbol "CAR")) '(car :inherited)))))))
(define-test sacla-must-package.96 (:tag :sacla)
 (assert-true
  (equal (multiple-value-list (find-symbol "NIL" 'cl)) '(nil :external))))
(define-test sacla-must-package.97 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let* ((*package* (make-package "TB-FOO" :use (list 'cl)))
           (symbol (intern "car" *package*)))
      (and
       (equal (multiple-value-list (find-symbol "car"))
              (list symbol :internal))
       (equal (multiple-value-list (find-symbol "CAR"))
              (list 'car :inherited)))))))
(define-test sacla-must-package.98 (:tag :sacla)
 (assert-true (member 'car (find-all-symbols 'car))))
(define-test sacla-must-package.99 (:tag :sacla)
 (assert-true (member 'cdr (find-all-symbols "CDR"))))
(define-test sacla-must-package.100 (:tag :sacla)
 (assert-true (every #'symbolp (find-all-symbols "LOOP"))))
(define-test sacla-must-package.101 (:tag :sacla)
 (assert-true
  (every #'(lambda (name) (string= name "FIND"))
         (mapcar #'symbol-name (find-all-symbols "FIND")))))
(define-test sacla-must-package.102 (:tag :sacla)
 (assert-true
  (dolist
      (name
       (list "CAR"
             "CDR"
             #\a
             #\A
             'common-lisp
             'join
             ""
             "XXX"
             "aA"
             "LONGLONGLONGLONGLONGLONGLONGLONGLONGLONG"
             'long-long-long-long-long-long-name)
       t)
    (unless
        (every #'(lambda (symbol-name) (string= symbol-name name))
               (mapcar #'symbol-name (find-all-symbols name)))
      (return nil)))))
(define-test sacla-must-package.103 (:tag :sacla)
 (assert-true (symbolp (intern "SYMBOL"))))
(define-test sacla-must-package.104 (:tag :sacla)
 (assert-true (symbolp (intern "long-long-name-in-lower-case"))))
(define-test sacla-must-package.105 (:tag :sacla)
 (assert-true
  (equal (multiple-value-list (intern "NIL" 'cl)) '(nil :external))))
(define-test sacla-must-package.106 (:tag :sacla)
 (assert-true
  (multiple-value-bind (boo status)
      (intern "BOO")
    (and (symbolp boo)
         (member status '(nil :internal :external :inherited))
         (string= (symbol-name boo) "BOO")))))
(define-test sacla-must-package.107 (:tag :sacla)
 (assert-true
  (let ((*package* (find-package "CL")))
    (equal (multiple-value-list (intern "CAR")) '(car :external)))))
(define-test sacla-must-package.108 (:tag :sacla)
 (assert-true
  (let ((*package* (find-package "KEYWORD")))
    (equal (multiple-value-list (intern "TEST")) '(:test :external)))))
(define-test sacla-must-package.109 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (make-package "TB-FOO" :use nil)
    (and (multiple-value-list (intern "BOO" 'tb-foo))
         (list (find-symbol "BOO" 'tb-foo) nil)
         (eq (symbol-package (find-symbol "BOO" 'tb-foo))
             (find-package 'tb-foo))))))
(define-test sacla-must-package.110 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use '(cl))))
      (and (eq (intern "CAR") 'car)
           (equal (multiple-value-list (intern "ZZZ"))
                  (list (find-symbol "ZZZ") nil))
           (equal (multiple-value-list (intern "ZZZ"))
                  (list (find-symbol "ZZZ") :internal))
           (export (find-symbol "ZZZ"))
           (equal (multiple-value-list (intern "ZZZ"))
                  (list (find-symbol "ZZZ") :external)))))))
(define-test sacla-must-package.111 (:tag :sacla)
 (assert-true (eq (export nil) t)))
(define-test sacla-must-package.112 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use nil)) buz)
      (and (setq buz (intern "BUZ"))
           (equal (multiple-value-list (find-symbol "BUZ"))
                  (list buz :internal))
           (eq (export buz) t)
           (equal (multiple-value-list (find-symbol "BUZ"))
                  (list buz :external)))))))
(define-test sacla-must-package.113 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use '(cl))))
      (and (equal (multiple-value-list (find-symbol "CAR")) '(car :inherited))
           (eq (export 'car) t)
           (equal (multiple-value-list (find-symbol "CAR"))
                  '(car :external)))))))
(define-test sacla-must-package.114 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use '(cl))))
      (and (equal (multiple-value-list (find-symbol "CAR")) '(car :inherited))
           (eq (export '(car)) t)
           (equal (multiple-value-list (find-symbol "CAR"))
                  '(car :external)))))))
(define-test sacla-must-package.115 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use '(cl))))
      (and (equal (multiple-value-list (find-symbol "CAR")) '(car :inherited))
           (equal (multiple-value-list (find-symbol "CDR")) '(cdr :inherited))
           (eq (export '(car cdr)) t)
           (equal (multiple-value-list (find-symbol "CAR")) '(car :external))
           (equal (multiple-value-list (find-symbol "CDR"))
                  '(cdr :external)))))))
(define-test sacla-must-package.116 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use '(cl)))
          (buz (make-symbol "BUZ")))
      (import buz)
      (and (equal (multiple-value-list (find-symbol "CAR")) '(car :inherited))
           (equal (multiple-value-list (find-symbol "CDR")) '(cdr :inherited))
           (equal (multiple-value-list (find-symbol "BUZ"))
                  (list buz :internal))
           (eq (export (list 'car buz 'cdr)) t)
           (equal (multiple-value-list (find-symbol "CAR")) '(car :external))
           (equal (multiple-value-list (find-symbol "CDR")) '(cdr :external))
           (equal (multiple-value-list (find-symbol "BUZ"))
                  (list buz :external)))))))
(define-test sacla-must-package.117 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "A")
      (delete-package "A"))
    (make-package "A" :use nil)
    (import 'car "A")
    (and (eq (export 'car "A") t)
         (equal (multiple-value-list (find-symbol "CAR" "A"))
                '(car :external))))))
(define-test sacla-must-package.118 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "A")
      (delete-package "A"))
    (make-package "A" :use nil)
    (import 'car "A")
    (and (eq (export 'car #\A) t)
         (equal (multiple-value-list (find-symbol "CAR" "A"))
                '(car :external))))))
(define-test sacla-must-package.119 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "A")
      (delete-package "A"))
    (make-package "A" :use nil)
    (import 'car "A")
    (and (eq (export 'car 'a) t)
         (equal (multiple-value-list (find-symbol "CAR" "A"))
                '(car :external))))))
(define-test sacla-must-package.120 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "A")
      (delete-package "A"))
    (make-package "A" :use nil)
    (import 'car "A")
    (and (eq (export 'car (find-package 'a)) t)
         (equal (multiple-value-list (find-symbol "CAR" "A"))
                '(car :external))))))
(define-test sacla-must-package.121 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use '(cl))))
      (and (equal (multiple-value-list (find-symbol "CAR")) '(car :inherited))
           (eq (export 'car) t)
           (equal (multiple-value-list (find-symbol "CAR")) '(car :external))
           (unuse-package 'cl)
           (equal (multiple-value-list (find-symbol "CAR"))
                  '(car :external)))))))
(define-test sacla-must-package.122 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE" :use nil)
    (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))
    (let ((buz (intern "BUZ" 'tb-bar-to-use)))
      (and (equal (multiple-value-list (find-symbol "BUZ" 'tb-foo)) '(nil nil))
           (export buz 'tb-bar-to-use)
           (equal (multiple-value-list (find-symbol "BUZ" 'tb-foo))
                  (list buz :inherited)))))))
(define-test sacla-must-package.123 (:tag :sacla)
 (assert-true
  (handler-case
      (progn
        (when (find-package "TB-FOO")
          (delete-package "TB-FOO"))
        (make-package "TB-FOO" :use nil)
        (export 'car "TB-FOO"))
    (package-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-package.124 (:tag :sacla)
 (assert-true
  (handler-case
      (progn
        (when (find-package "TB-FOO")
          (delete-package "TB-FOO"))
        (when (find-package "TB-BAR-TO-USE")
          (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
          (delete-package "TB-BAR-TO-USE"))
        (make-package "TB-BAR-TO-USE" :use nil)
        (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))
        (intern "BUZ" 'tb-foo)
        (let ((buz (intern "BUZ" 'tb-bar-to-use)))
          (export buz 'tb-bar-to-use)))
    (package-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-package.125 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use nil)) buz)
      (and (export (setq buz (intern "BUZ")))
           (equal (multiple-value-list (find-symbol "BUZ"))
                  (list buz :external))
           (eq (unexport buz) t)
           (equal (multiple-value-list (find-symbol "BUZ"))
                  (list buz :internal)))))))
(define-test sacla-must-package.126 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "A")
      (delete-package "A"))
    (make-package "A" :use nil)
    (let (buz)
      (and (export (setq buz (intern "BUZ" 'a)) 'a)
           (equal (multiple-value-list (find-symbol "BUZ" 'a))
                  (list buz :external))
           (eq (unexport buz 'a) t)
           (equal (multiple-value-list (find-symbol "BUZ" 'a))
                  (list buz :internal)))))))
(define-test sacla-must-package.127 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "A")
      (delete-package "A"))
    (make-package "A" :use nil)
    (let (buz)
      (and (export (setq buz (intern "BUZ" 'a)) 'a)
           (equal (multiple-value-list (find-symbol "BUZ" 'a))
                  (list buz :external))
           (eq (unexport buz #\A) t)
           (equal (multiple-value-list (find-symbol "BUZ" 'a))
                  (list buz :internal)))))))
(define-test sacla-must-package.128 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "A")
      (delete-package "A"))
    (make-package "A" :use nil)
    (let (buz)
      (and (export (setq buz (intern "BUZ" 'a)) 'a)
           (equal (multiple-value-list (find-symbol "BUZ" 'a))
                  (list buz :external))
           (eq (unexport buz "A") t)
           (equal (multiple-value-list (find-symbol "BUZ" 'a))
                  (list buz :internal)))))))
(define-test sacla-must-package.129 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "A")
      (delete-package "A"))
    (make-package "A" :use nil)
    (let (buz)
      (and (export (setq buz (intern "BUZ" 'a)) 'a)
           (equal (multiple-value-list (find-symbol "BUZ" 'a))
                  (list buz :external))
           (eq (unexport buz (find-package "A")) t)
           (equal (multiple-value-list (find-symbol "BUZ" 'a))
                  (list buz :internal)))))))
(define-test sacla-must-package.130 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (make-package "TB-FOO" :use nil)
    (let (buz)
      (and (export (setq buz (intern "BUZ" 'tb-foo)) 'tb-foo)
           (equal (multiple-value-list (find-symbol "BUZ" 'tb-foo))
                  (list buz :external))
           (eq (unexport buz 'tb-foo) t)
           (equal (multiple-value-list (find-symbol "BUZ" 'tb-foo))
                  (list buz :internal)))))))
(define-test sacla-must-package.131 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let* ((*package* (make-package "TB-FOO" :use nil))
           (names '("A" "BC" "DEF" "GHIJ"))
           (symbols (mapcar #'intern names)))
      (and (export symbols)
           (eq (unexport symbols) t)
           (every #'(lambda (status) (eq status :internal))
                  (mapcar
                   #'(lambda (name)
                       (cadr (multiple-value-list (find-symbol name))))
                   names)))))))
(define-test sacla-must-package.132 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let* ((*package* (make-package "TB-FOO" :use nil)))
      (import '(nil))
      (export '(nil))
      (and (eq (unexport 'nil) t)
           (equal (multiple-value-list (find-symbol "NIL"))
                  '(nil :external)))))))
(define-test sacla-must-package.133 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let* ((*package* (make-package "TB-FOO" :use nil)))
      (import '(nil))
      (export '(nil))
      (and (eq (unexport '(nil)) t)
           (equal (multiple-value-list (find-symbol "NIL"))
                  '(nil :internal)))))))
(define-test sacla-must-package.134 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let* ((*package* (make-package "TB-FOO" :use nil))
           (baz (intern "BAZ" *package*)))
      (and
       (equal (multiple-value-list (find-symbol "BAZ")) (list baz :internal))
       (eq (unexport (list baz) *package*) t)
       (equal (multiple-value-list (find-symbol "BAZ"))
              (list baz :internal)))))))
(define-test sacla-must-package.135 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let* ((*package* (make-package "TB-FOO" :use nil))
           (baz (intern "BAZ" *package*))
           (woo (intern "WOO" *package*)))
      (export woo)
      (and
       (equal (multiple-value-list (find-symbol "BAZ")) (list baz :internal))
       (equal (multiple-value-list (find-symbol "WOO")) (list woo :external))
       (eq (unexport (list baz woo) *package*) t)
       (equal (multiple-value-list (find-symbol "BAZ")) (list baz :internal))
       (equal (multiple-value-list (find-symbol "WOO"))
              (list woo :internal)))))))
(define-test sacla-must-package.136 (:tag :sacla)
 (assert-true
  (handler-case
      (progn
        (when (find-package "TB-FOO")
          (delete-package "TB-FOO"))
        (let* ((*package* (make-package "TB-FOO" :use nil)))
          (unexport 'car)))
    (package-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-package.137 (:tag :sacla)
 (assert-true
  (handler-case
      (progn
        (when (find-package "TB-FOO")
          (delete-package "TB-FOO"))
        (let* ((*package* (make-package "TB-FOO" :use nil))
               (baz (intern "BAZ" *package*))
               (woo (intern "WOO" *package*)))
          (export woo)
          (unexport (list baz 'nil woo) *package*)))
    (package-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-package.138 (:tag :sacla)
 (assert-true (eq (shadow 'nil) t)))
(define-test sacla-must-package.139 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (make-package "TB-FOO" :use nil)
    (and (eq (shadow "A" 'tb-foo) t)
         (eq (cadr (multiple-value-list (find-symbol "A" 'tb-foo))) :internal)
         (equal (package-shadowing-symbols 'tb-foo)
                (list (find-symbol "A" 'tb-foo)))))))
(define-test sacla-must-package.140 (:tag :sacla)
 (assert-true (eq (shadow 'nil) t)))
(define-test sacla-must-package.141 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (make-package "TB-FOO" :use nil)
    (and (eq (shadow #\A 'tb-foo) t)
         (eq (cadr (multiple-value-list (find-symbol "A" 'tb-foo))) :internal)
         (equal (package-shadowing-symbols 'tb-foo)
                (list (find-symbol "A" 'tb-foo)))))))
(define-test sacla-must-package.142 (:tag :sacla)
 (assert-true (eq (shadow 'nil) t)))
(define-test sacla-must-package.143 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (make-package "TB-FOO" :use nil)
    (and (eq (shadow 'a 'tb-foo) t)
         (eq (cadr (multiple-value-list (find-symbol "A" 'tb-foo))) :internal)
         (equal (package-shadowing-symbols 'tb-foo)
                (list (find-symbol "A" 'tb-foo)))))))
(define-test sacla-must-package.144 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (make-package "TB-FOO" :use nil)
    (and (eq (shadow '(a) 'tb-foo) t)
         (eq (cadr (multiple-value-list (find-symbol "A" 'tb-foo))) :internal)
         (equal (package-shadowing-symbols 'tb-foo)
                (list (find-symbol "A" 'tb-foo)))))))
(define-test sacla-must-package.145 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (make-package "TB-FOO" :use nil)
    (and (eq (shadow '("A") 'tb-foo) t)
         (eq (cadr (multiple-value-list (find-symbol "A" 'tb-foo))) :internal)
         (equal (package-shadowing-symbols 'tb-foo)
                (list (find-symbol "A" 'tb-foo)))))))
(define-test sacla-must-package.146 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (make-package "TB-FOO" :use nil)
    (and (eq (shadow '(#\A) 'tb-foo) t)
         (eq (cadr (multiple-value-list (find-symbol "A" 'tb-foo))) :internal)
         (equal (package-shadowing-symbols 'tb-foo)
                (list (find-symbol "A" 'tb-foo)))))))
(define-test sacla-must-package.147 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "A")
      (delete-package "A"))
    (make-package "A" :use nil)
    (and (eq (shadow "BUZ" #\A) t)
         (eq (cadr (multiple-value-list (find-symbol "BUZ" 'a))) :internal)
         (equal (package-shadowing-symbols 'a)
                (list (find-symbol "BUZ" 'a)))))))
(define-test sacla-must-package.148 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "A")
      (delete-package "A"))
    (make-package "A" :use nil)
    (and (eq (shadow "BUZ" "A") t)
         (eq (cadr (multiple-value-list (find-symbol "BUZ" 'a))) :internal)
         (equal (package-shadowing-symbols 'a)
                (list (find-symbol "BUZ" 'a)))))))
(define-test sacla-must-package.149 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "A")
      (delete-package "A"))
    (make-package "A" :use nil)
    (and (eq (shadow "BUZ" 'a) t)
         (eq (cadr (multiple-value-list (find-symbol "BUZ" 'a))) :internal)
         (equal (package-shadowing-symbols 'a)
                (list (find-symbol "BUZ" 'a)))))))
(define-test sacla-must-package.150 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "A")
      (delete-package "A"))
    (make-package "A" :use nil)
    (and (eq (shadow "BUZ" (find-package 'a)) t)
         (eq (cadr (multiple-value-list (find-symbol "BUZ" 'a))) :internal)
         (equal (package-shadowing-symbols 'a)
                (list (find-symbol "BUZ" 'a)))))))
(define-test sacla-must-package.151 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use nil))
          (names '(a #\B "C" "BUZ")))
      (and (eq (shadow names) t)
           (every
            #'(lambda (name)
                (eq (cadr (multiple-value-list (find-symbol name))) :internal))
            names)
           (null
            (set-difference (mapcar #'find-symbol (mapcar #'string names))
                            (package-shadowing-symbols *package*))))))))
(define-test sacla-must-package.152 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use '(cl)))
          (names '(a #\B "C" "BUZ" "CAR"))
          a
          b
          c)
      (setq a (intern "A"))
      (export (setq b (intern "B")))
      (shadowing-import (setq c (intern "C")))
      (and (eq (shadow names) t)
           (equal (multiple-value-list (find-symbol "A")) (list a :internal))
           (equal (multiple-value-list (find-symbol "B")) (list b :external))
           (equal (multiple-value-list (find-symbol "C")) (list c :internal))
           (eq (cadr (multiple-value-list (find-symbol "BUZ"))) :internal)
           (eq (cadr (multiple-value-list (find-symbol "CAR"))) :internal)
           (not (eq (car (multiple-value-list (find-symbol "CAR"))) 'car))
           (null
            (set-difference (mapcar #'find-symbol (mapcar #'string names))
                            (package-shadowing-symbols *package*))))))))
(define-test sacla-must-package.153 (:tag :sacla)
 (assert-true (eq (shadowing-import 'nil) t)))
(define-test sacla-must-package.154 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (shadowing-import 'nil (make-package "TB-FOO" :use nil))
    (let ((list nil))
      (null (do-symbols (symbol "TB-FOO" list) (push symbol list)))))))
(define-test sacla-must-package.155 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use nil)))
      (and (not (find-symbol "CAR"))
           (not (find-symbol "CDR"))
           (not (find-symbol "LIST"))
           (eq (shadowing-import '(car cdr list)) t)
           (eq (find-symbol "CAR") 'car)
           (eq (find-symbol "CDR") 'cdr)
           (eq (find-symbol "LIST") 'list))))))
(define-test sacla-must-package.156 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let* ((*package* (make-package "TB-FOO" :use (list 'cl)))
           (names '("CAR" "CDR" "LIST" "APPEND"))
           (symbols (mapcar #'make-symbol names)))
      (and (eq (shadowing-import symbols) t)
           (every #'eq symbols (mapcar #'find-symbol names))
           (every
            #'(lambda (symbol)
                (member symbol (package-shadowing-symbols *package*)))
            symbols))))))
(define-test sacla-must-package.157 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "A")
      (delete-package "A"))
    (make-package "A" :use nil)
    (let ((symbol (make-symbol "CAR")))
      (and (eq (shadowing-import symbol "A") t)
           (equal (multiple-value-list (find-symbol "CAR" "A"))
                  (list symbol :internal)))))))
(define-test sacla-must-package.158 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "A")
      (delete-package "A"))
    (make-package "A" :use nil)
    (let ((symbol (make-symbol "CAR")))
      (and (eq (shadowing-import symbol #\A) t)
           (equal (multiple-value-list (find-symbol "CAR" "A"))
                  (list symbol :internal)))))))
(define-test sacla-must-package.159 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "A")
      (delete-package "A"))
    (make-package "A" :use nil)
    (let ((symbol (make-symbol "CAR")))
      (and (eq (shadowing-import symbol 'a) t)
           (equal (multiple-value-list (find-symbol "CAR" "A"))
                  (list symbol :internal)))))))
(define-test sacla-must-package.160 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "A")
      (delete-package "A"))
    (make-package "A" :use nil)
    (let ((symbol (make-symbol "CAR")))
      (and (eq (shadowing-import symbol (find-package 'a)) t)
           (equal (multiple-value-list (find-symbol "CAR" "A"))
                  (list symbol :internal)))))))
(define-test sacla-must-package.161 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (make-package "TB-FOO" :use nil)
    (let ((buz0 (intern "BUZ" 'tb-foo)) (buz1 (make-symbol "BUZ")))
      (and
       (equal (multiple-value-list (find-symbol "BUZ" 'tb-foo))
              (list buz0 :internal))
       (eq (shadowing-import buz1 'tb-foo) t)
       (equal (multiple-value-list (find-symbol "BUZ" 'tb-foo))
              (list buz1 :internal))
       (equal (list buz1) (package-shadowing-symbols 'tb-foo))
       (unintern buz1 'tb-foo)
       (equal (multiple-value-list (find-symbol "BUZ" 'tb-foo)) '(nil nil))
       (null (package-shadowing-symbols 'tb-foo)))))))
(define-test sacla-must-package.162 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (make-package "TB-FOO" :use nil)
    (let ((buz0 (intern "BUZ" 'tb-foo)) (buz1 (make-symbol "BUZ")))
      (shadow buz0 'tb-foo)
      (and
       (equal (multiple-value-list (find-symbol "BUZ" 'tb-foo))
              (list buz0 :internal))
       (eq (shadowing-import buz1 'tb-foo) t)
       (equal (multiple-value-list (find-symbol "BUZ" 'tb-foo))
              (list buz1 :internal))
       (equal (list buz1) (package-shadowing-symbols 'tb-foo))
       (unintern buz1 'tb-foo)
       (equal (multiple-value-list (find-symbol "BUZ" 'tb-foo)) '(nil nil))
       (null (package-shadowing-symbols 'tb-foo)))))))
(define-test sacla-must-package.163 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (make-package "TB-FOO" :use nil)
    (let ((buz0 (intern "BUZ" 'tb-foo)) (buz1 (make-symbol "BUZ")))
      (export buz0 'tb-foo)
      (and
       (equal (multiple-value-list (find-symbol "BUZ" 'tb-foo))
              (list buz0 :external))
       (eq (shadowing-import buz1 'tb-foo) t)
       (equal (multiple-value-list (find-symbol "BUZ" 'tb-foo))
              (list buz1 :internal))
       (equal (list buz1) (package-shadowing-symbols 'tb-foo))
       (unintern buz1 'tb-foo)
       (equal (multiple-value-list (find-symbol "BUZ" 'tb-foo)) '(nil nil))
       (null (package-shadowing-symbols 'tb-foo)))))))
(define-test sacla-must-package.164 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (make-package "TB-FOO" :use nil)
    (let ((buz0 (intern "BUZ" 'tb-foo)) (buz1 (make-symbol "BUZ")))
      (export buz0 'tb-foo)
      (shadow buz0 'tb-foo)
      (and
       (equal (multiple-value-list (find-symbol "BUZ" 'tb-foo))
              (list buz0 :external))
       (eq (shadowing-import buz1 'tb-foo) t)
       (equal (multiple-value-list (find-symbol "BUZ" 'tb-foo))
              (list buz1 :internal))
       (equal (list buz1) (package-shadowing-symbols 'tb-foo))
       (unintern buz1 'tb-foo)
       (equal (multiple-value-list (find-symbol "BUZ" 'tb-foo)) '(nil nil))
       (null (package-shadowing-symbols 'tb-foo)))))))
(define-test sacla-must-package.165 (:tag :sacla)
 (assert-true (eq (import 'nil) t)))
(define-test sacla-must-package.166 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (make-package "TB-FOO" :use nil)
    (let ((list nil))
      (and (eq (import 'nil "TB-FOO") t)
           (null (do-symbols (symbol "TB-FOO" list) (push symbol list))))))))
(define-test sacla-must-package.167 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "A")
      (delete-package "A"))
    (make-package "A" :use nil)
    (and (not (find-symbol "CAR" 'a))
         (eq (import 'car 'a) t)
         (equal (multiple-value-list (find-symbol "CAR" 'a))
                '(car :internal))))))
(define-test sacla-must-package.168 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "A")
      (delete-package "A"))
    (make-package "A" :use nil)
    (and (not (find-symbol "CAR" 'a))
         (eq (import 'car #\A) t)
         (equal (multiple-value-list (find-symbol "CAR" 'a))
                '(car :internal))))))
(define-test sacla-must-package.169 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "A")
      (delete-package "A"))
    (make-package "A" :use nil)
    (and (not (find-symbol "CAR" 'a))
         (eq (import 'car "A") t)
         (equal (multiple-value-list (find-symbol "CAR" 'a))
                '(car :internal))))))
(define-test sacla-must-package.170 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "A")
      (delete-package "A"))
    (make-package "A" :use nil)
    (and (not (find-symbol "CAR" 'a))
         (eq (import 'car (find-package "A")) t)
         (equal (multiple-value-list (find-symbol "CAR" 'a))
                '(car :internal))))))
(define-test sacla-must-package.171 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (make-package "TB-FOO" :use nil)
    (and (not (find-symbol "CAR" 'tb-foo))
         (eq (import 'car 'tb-foo) t)
         (equal (multiple-value-list (find-symbol "CAR" 'tb-foo))
                '(car :internal))))))
(define-test sacla-must-package.172 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (make-package "TB-FOO" :use nil)
    (and (not (find-symbol "CAR" 'tb-foo))
         (eq (import (list 'car 'cdr 'list :test) 'tb-foo) t)
         (equal (multiple-value-list (find-symbol "CAR" 'tb-foo))
                '(car :internal))
         (equal (multiple-value-list (find-symbol "CDR" 'tb-foo))
                '(cdr :internal))
         (equal (multiple-value-list (find-symbol "TEST" 'tb-foo))
                '(:test :internal))))))
(define-test sacla-must-package.173 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use nil)))
      (and (not (find-symbol "CAR" 'tb-foo))
           (eq (import (list 'car 'cdr 'list :test)) t)
           (equal (multiple-value-list (find-symbol "CAR" 'tb-foo))
                  '(car :internal))
           (equal (multiple-value-list (find-symbol "CDR" 'tb-foo))
                  '(cdr :internal))
           (equal (multiple-value-list (find-symbol "TEST" 'tb-foo))
                  '(:test :internal)))))))
(define-test sacla-must-package.174 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let (buz)
      (make-package "TB-FOO" :use nil)
      (and (export (setq buz (intern "BUZ" "TB-FOO")) "TB-FOO")
           (equal (multiple-value-list (find-symbol "BUZ" "TB-FOO"))
                  (list buz :external))
           (eq (import buz "TB-FOO") t)
           (equal (multiple-value-list (find-symbol "BUZ" "TB-FOO"))
                  (list buz :external)))))))
(define-test sacla-must-package.175 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let (buz)
      (make-package "TB-FOO" :use nil)
      (and (setq buz (intern "BUZ" "TB-FOO"))
           (equal (multiple-value-list (find-symbol "BUZ" "TB-FOO"))
                  (list buz :internal))
           (eq (import buz "TB-FOO") t)
           (equal (multiple-value-list (find-symbol "BUZ" "TB-FOO"))
                  (list buz :internal)))))))
(define-test sacla-must-package.176 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use '(cl))))
      (and (equal (multiple-value-list (find-symbol "CAR")) '(car :inherited))
           (eq (import 'car) t)
           (equal (multiple-value-list (find-symbol "CAR"))
                  '(car :internal)))))))
(define-test sacla-must-package.177 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (make-package "TB-FOO" :use nil)
    (let ((buz (make-symbol "BUZ")))
      (and (null (symbol-package buz))
           (eq (import buz 'tb-foo) t)
           (eq (symbol-package buz) (find-package 'tb-foo)))))))
(define-test sacla-must-package.178 (:tag :sacla)
 (assert-true
  (handler-case
      (progn
        (when (find-package "TB-FOO")
          (delete-package "TB-FOO"))
        (let ((*package* (make-package "TB-FOO" :use '(cl))))
          (import (make-symbol "CAR"))))
    (package-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-package.179 (:tag :sacla)
 (assert-true
  (handler-case
      (progn
        (when (find-package "TB-FOO")
          (delete-package "TB-FOO"))
        (let ((*package* (make-package "TB-FOO" :use nil)))
          (intern "BUZ")
          (import (make-symbol "BUZ"))))
    (package-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-package.180 (:tag :sacla)
 (assert-true
  (handler-case
      (progn
        (when (find-package "TB-FOO")
          (delete-package "TB-FOO"))
        (let ((*package* (make-package "TB-FOO" :use nil)))
          (export (intern "BUZ"))
          (import (make-symbol "BUZ"))))
    (package-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-package.181 (:tag :sacla)
 (assert-true
  (handler-case
      (progn
        (when (find-package "TB-FOO")
          (delete-package "TB-FOO"))
        (let ((*package* (make-package "TB-FOO" :use nil)))
          (shadowing-import (make-symbol "BUZ"))
          (import (make-symbol "BUZ"))))
    (package-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-package.182 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (make-package "TB-FOO" :use nil)
    (not (unintern 'car "TB-FOO")))))
(define-test sacla-must-package.183 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (make-package "TB-FOO" :use nil)
    (and (unintern (intern "BUZ" "TB-FOO") "TB-FOO")
         (not (find-symbol "BUZ" "TB-FOO"))))))
(define-test sacla-must-package.184 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use nil)))
      (not (unintern 'car))))))
(define-test sacla-must-package.185 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use nil)))
      (and (unintern (intern "BUZ")) (not (find-symbol "BUZ")))))))
(define-test sacla-must-package.186 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "A")
      (delete-package "A"))
    (make-package "A" :use nil)
    (and (unintern (intern "BUZ" "A") #\A) (not (find-symbol "BUZ" "A"))))))
(define-test sacla-must-package.187 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "A")
      (delete-package "A"))
    (make-package "A" :use nil)
    (and (unintern (intern "BUZ" "A") "A") (not (find-symbol "BUZ" "A"))))))
(define-test sacla-must-package.188 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "A")
      (delete-package "A"))
    (make-package "A" :use nil)
    (and (unintern (intern "BUZ" "A") 'a) (not (find-symbol "BUZ" "A"))))))
(define-test sacla-must-package.189 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "A")
      (delete-package "A"))
    (make-package "A" :use nil)
    (and (unintern (intern "BUZ" "A") (find-package 'a))
         (not (find-symbol "BUZ" "A"))))))
(define-test sacla-must-package.190 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use '(cl))))
      (and (equal (multiple-value-list (find-symbol "CAR")) '(car :inherited))
           (not (unintern 'car)))))))
(define-test sacla-must-package.191 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use nil)))
      (and (import 'car)
           (equal (multiple-value-list (find-symbol "CAR")) '(car :internal))
           (unintern 'car)
           (not (find-symbol "CAR")))))))
(define-test sacla-must-package.192 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use '(cl))))
      (and (equal (multiple-value-list (find-symbol "CAR")) '(car :inherited))
           (import 'car)
           (equal (multiple-value-list (find-symbol "CAR")) '(car :internal))
           (unintern 'car)
           (equal (multiple-value-list (find-symbol "CAR"))
                  '(car :inherited)))))))
(define-test sacla-must-package.193 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use nil))
          (buz (make-symbol "BUZ")))
      (and (null (symbol-package buz))
           (import buz)
           (shadow buz)
           (eq (symbol-package buz) *package*)
           (member buz (package-shadowing-symbols *package*))
           (unintern buz)
           (not (find-symbol "BUZ"))
           (not (member buz (package-shadowing-symbols *package*)))
           (null (symbol-package buz)))))))
(define-test sacla-must-package.194 (:tag :sacla)
 (assert-true
  (handler-case
      (progn
        (when (find-package "TB-FOO")
          (delete-package "TB-FOO"))
        (when (find-package "TB-BAR-TO-USE")
          (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
          (delete-package "TB-BAR-TO-USE"))
        (let ((*package* (make-package "TB-FOO" :use nil)) symbol)
          (and (setq symbol (intern "CAR"))
               (shadow "CAR")
               (make-package "TB-BAR-TO-USE" :use nil)
               (export (intern "CAR" "TB-BAR-TO-USE") "TB-BAR-TO-USE")
               (use-package (list "TB-BAR-TO-USE" "CL"))
               (equal (multiple-value-list (find-symbol "CAR"))
                      (list symbol :internal))
               (unintern symbol))))
    (package-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-package.195 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (let ((*package* (make-package "TB-FOO" :use nil)) symbol)
      (and (setq symbol (intern "CAR"))
           (shadow "CAR")
           (make-package "TB-BAR-TO-USE" :use nil)
           (import 'car "TB-BAR-TO-USE")
           (export 'car "TB-BAR-TO-USE")
           (use-package (list "TB-BAR-TO-USE" "CL"))
           (equal (multiple-value-list (find-symbol "CAR"))
                  (list symbol :internal))
           (unintern symbol))))))
(define-test sacla-must-package.196 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use nil)))
      (and (not (find-symbol "CAR"))
           (eq (use-package 'cl) t)
           (find-symbol "CAR"))))))
(define-test sacla-must-package.197 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use nil)))
      (and (not (find-symbol "CAR"))
           (eq (use-package "COMMON-LISP") t)
           (find-symbol "CAR"))))))
(define-test sacla-must-package.198 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use nil)))
      (and (not (find-symbol "CAR"))
           (eq (use-package (find-package "COMMON-LISP")) t)
           (find-symbol "CAR"))))))
(define-test sacla-must-package.199 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use nil)))
      (and (not (find-symbol "CAR"))
           (eq (use-package '(cl)) t)
           (find-symbol "CAR"))))))
(define-test sacla-must-package.200 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use nil)))
      (and (not (find-symbol "CAR"))
           (eq (use-package '("COMMON-LISP")) t)
           (find-symbol "CAR"))))))
(define-test sacla-must-package.201 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use nil)))
      (and (not (find-symbol "CAR"))
           (eq (use-package (list (find-package "COMMON-LISP"))) t)
           (find-symbol "CAR"))))))
(define-test sacla-must-package.202 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((package (make-package "TB-FOO" :use nil))
          (*package* (find-package 'cl-user)))
      (and (not (find-symbol "CAR" package))
           (eq (use-package (list (find-package "COMMON-LISP")) package) t)
           (find-symbol "CAR" package))))))
(define-test sacla-must-package.203 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((package (make-package "TB-FOO" :use nil))
          (*package* (find-package 'cl-user)))
      (and (not (find-symbol "CAR" package))
           (eq (use-package (list (find-package "COMMON-LISP")) "TB-FOO") t)
           (find-symbol "CAR" package))))))
(define-test sacla-must-package.204 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((package (make-package "TB-FOO" :use nil))
          (*package* (find-package 'cl-user)))
      (and (not (find-symbol "CAR" package))
           (eq (use-package (list (find-package "COMMON-LISP")) 'tb-foo) t)
           (find-symbol "CAR" package))))))
(define-test sacla-must-package.205 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((package (make-package "TB-FOO" :use nil))
          (*package* (find-package 'cl-user)))
      (and (not (find-symbol "CAR" package))
           (eq
            (use-package (list (find-package "COMMON-LISP"))
                         (find-package 'tb-foo))
            t)
           (find-symbol "CAR" package))))))
(define-test sacla-must-package.206 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use nil)))
      (and (use-package 'cl)
           (member (find-package 'cl) (package-use-list *package*)))))))
(define-test sacla-must-package.207 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (let* ((*package* (make-package "TB-FOO" :use nil)) boo woo buz)
      (and (make-package "TB-BAR-TO-USE" :use nil)
           (export (list (setq boo (intern "BOO" 'tb-bar-to-use)))
                   'tb-bar-to-use)
           (setq woo (intern "WOO"))
           (export (list (setq buz (intern "BUZ"))))
           (use-package (list 'tb-bar-to-use 'cl))
           (equal (multiple-value-list (find-symbol "BOO"))
                  (list boo :inherited))
           (equal (multiple-value-list (find-symbol "WOO"))
                  (list woo :internal))
           (equal (multiple-value-list (find-symbol "BUZ"))
                  (list buz :external))
           (equal (multiple-value-list (find-symbol "CAR"))
                  (list 'car :inherited))
           (equal (multiple-value-list (find-symbol "LIST"))
                  (list 'list :inherited)))))))
(define-test sacla-must-package.208 (:tag :sacla)
 (assert-true
  (handler-case
      (progn
        (when (find-package "TB-FOO")
          (delete-package "TB-FOO"))
        (make-package "TB-FOO" :use nil)
        (intern "CAR" 'tb-foo)
        (use-package 'cl 'tb-foo))
    (package-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-package.209 (:tag :sacla)
 (assert-true
  (handler-case
      (progn
        (when (find-package "TB-FOO")
          (delete-package "TB-FOO"))
        (make-package "TB-FOO" :use nil)
        (export (intern "CAR" 'tb-foo) 'tb-foo)
        (use-package 'cl 'tb-foo))
    (package-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-package.210 (:tag :sacla)
 (assert-true
  (handler-case
      (progn
        (when (find-package "TB-FOO")
          (delete-package "TB-FOO"))
        (when (find-package "TB-BAR-TO-USE")
          (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
          (delete-package "TB-BAR-TO-USE"))
        (make-package "TB-FOO" :use '(cl))
        (make-package "TB-BAR-TO-USE" :use nil)
        (export (intern "CAR" 'tb-bar-to-use) 'tb-bar-to-use)
        (use-package 'tb-bar-to-use 'tb-foo))
    (package-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-package.211 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO-TO-USE")
      (unuse-package (package-use-list "TB-FOO-TO-USE") "TB-FOO-TO-USE"))
    (when (find-package "TB-BAR-TO-USE")
      (unuse-package (package-use-list "TB-BAR-TO-USE") "TB-BAR-TO-USE"))
    (when (find-package "TB-FOO-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-FOO-TO-USE"))
      (delete-package "TB-FOO-TO-USE"))
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (and (make-package "TB-FOO-TO-USE" :use nil)
         (make-package "TB-BAR-TO-USE" :use '("TB-FOO-TO-USE"))
         (use-package "TB-BAR-TO-USE" "TB-FOO-TO-USE")
         (export (intern "FOO" "TB-FOO-TO-USE") "TB-FOO-TO-USE")
         (export (intern "BAR" "TB-BAR-TO-USE") "TB-BAR-TO-USE")
         (eq (cadr (multiple-value-list (find-symbol "FOO" "TB-FOO-TO-USE")))
             :external)
         (eq (cadr (multiple-value-list (find-symbol "BAR" "TB-FOO-TO-USE")))
             :inherited)
         (eq (cadr (multiple-value-list (find-symbol "FOO" "TB-BAR-TO-USE")))
             :inherited)
         (eq (cadr (multiple-value-list (find-symbol "BAR" "TB-BAR-TO-USE")))
             :external)
         (unuse-package (package-use-list "TB-FOO-TO-USE") "TB-FOO-TO-USE")
         (unuse-package (package-use-list "TB-BAR-TO-USE") "TB-BAR-TO-USE")))))
(define-test sacla-must-package.212 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "a")
      (delete-package "a"))
    (and (make-package "a" :use nil)
         (delete-package "a")
         (not (find-package "a"))))))
(define-test sacla-must-package.213 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "a")
      (delete-package "a"))
    (and (make-package "a" :use nil)
         (delete-package #\a)
         (not (find-package "a"))))))
(define-test sacla-must-package.214 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "a")
      (delete-package "a"))
    (and (make-package "a" :use nil)
         (delete-package '|a|)
         (not (find-package "a"))))))
(define-test sacla-must-package.215 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "a")
      (delete-package "a"))
    (and (make-package "a" :use nil)
         (delete-package (find-package '|a|))
         (not (find-package "a"))))))
(define-test sacla-must-package.216 (:tag :sacla)
 (assert-true
  (progn
    (mapc
     #'(lambda (name)
         (when (find-package name)
           (delete-package name)))
     '("a" "b" "c" "d" "e"))
    (and (make-package "a" :nicknames '("b" "c" "d" "e") :use nil)
         (delete-package "a")
         (not (find-package "a"))
         (not (find-package "b"))
         (not (find-package "c"))
         (not (find-package "d"))
         (not (find-package "e"))))))
(define-test sacla-must-package.217 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((package (make-package "TB-FOO" :use nil)))
      (and (delete-package "TB-FOO")
           (not (find-package "TB-FOO"))
           (packagep package)
           (null (package-name package)))))))
(define-test sacla-must-package.218 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((package (make-package "TB-FOO" :use nil)))
      (and (delete-package "TB-FOO")
           (not (member package (list-all-packages))))))))
(define-test sacla-must-package.219 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((package (make-package "TB-FOO" :use nil)))
      (and (delete-package "TB-FOO") (null (delete-package package)))))))
(define-test sacla-must-package.220 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((car-home-package (symbol-package 'car)))
      (and (make-package "TB-FOO" :use nil)
           (import 'car "TB-FOO")
           (delete-package 'tb-foo)
           (eq 'car (find-symbol "CAR" 'cl))
           (eq (symbol-package 'car) car-home-package)
           (eq (intern "CAR" 'cl) 'car))))))
(define-test sacla-must-package.221 (:tag :sacla)
 (assert-true
  (handler-case
      (progn
        (when (find-package "TB-FOO")
          (delete-package "TB-FOO"))
        (when (find-package "TB-BAR-TO-USE")
          (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
          (delete-package "TB-BAR-TO-USE"))
        (and (make-package "TB-BAR-TO-USE" :use nil)
             (make-package "TB-FOO" :use '("TB-BAR-TO-USE"))
             (delete-package "TB-BAR-TO-USE")))
    (package-error nil t)
    (error nil nil)
    (:no-error (&rest rest) (declare (ignore rest)) nil))))
(define-test sacla-must-package.222 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use nil)))
      (in-package cl-user)
      (eq *package* (find-package 'cl-user))))))
(define-test sacla-must-package.223 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use nil)))
      (in-package "CL-USER")
      (eq *package* (find-package 'cl-user))))))
(define-test sacla-must-package.224 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "A")
      (delete-package "A"))
    (make-package "A" :use nil)
    (let ((*package* *package*))
      (in-package "A")
      (eq *package* (find-package 'a))))))
(define-test sacla-must-package.225 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "A")
      (delete-package "A"))
    (make-package "A" :use nil)
    (let ((*package* *package*))
      (in-package #\A)
      (eq *package* (find-package 'a))))))
(define-test sacla-must-package.226 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "A")
      (delete-package "A"))
    (make-package "A" :use nil)
    (let ((*package* *package*))
      (in-package a)
      (eq *package* (find-package 'a))))))
(define-test sacla-must-package.227 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "A")
      (delete-package "A"))
    (handler-case
        (progn
          (in-package "A"))
      (package-error nil t)
      (error nil nil)
      (:no-error (&rest rest) (declare (ignore rest)) nil)))))
(define-test sacla-must-package.228 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "A")
      (delete-package "A"))
    (packagep (defpackage #\A)))))
(define-test sacla-must-package.229 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "A")
      (delete-package "A"))
    (packagep (defpackage a)))))
(define-test sacla-must-package.230 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "A")
      (delete-package "A"))
    (packagep (defpackage "A")))))
(define-test sacla-must-package.231 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and (packagep (defpackage "TB-FOO"))
         (null (package-nicknames 'tb-foo))
         (null (package-shadowing-symbols 'tb-foo))))))
(define-test sacla-must-package.232 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and
     (packagep
      (defpackage "TB-FOO"
        (:nicknames)))
     (null (package-nicknames 'tb-foo))))))
(define-test sacla-must-package.233 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and
     (packagep
      (defpackage "TB-FOO"
        (:nicknames)
        (:shadow)))
     (null (package-nicknames 'tb-foo))
     (null (package-shadowing-symbols 'tb-foo))))))
(define-test sacla-must-package.234 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and
     (packagep
      (defpackage "TB-FOO"
        (:nicknames)
        (:shadow)
        (:shadowing-import-from common-lisp)))
     (null (package-nicknames 'tb-foo))
     (null (package-shadowing-symbols 'tb-foo))))))
(define-test sacla-must-package.235 (:tag :sacla)
 (assert-true
  (progn
    (mapc
     #'(lambda (name)
         (when (find-package name)
           (delete-package name)))
     '("TB-FOO" "TB-FOO-NICKNAME-1" "TB-FOO-NICKNAME-2" "TB-FOO-NICKNAME-3"))
    (and
     (packagep
      (defpackage "TB-FOO"
        (:nicknames tb-foo-nickname-1)))
     (equal (package-nicknames 'tb-foo) '("TB-FOO-NICKNAME-1"))))))
(define-test sacla-must-package.236 (:tag :sacla)
 (assert-true
  (progn
    (mapc
     #'(lambda (name)
         (when (find-package name)
           (delete-package name)))
     '("TB-FOO" "TB-FOO-NICKNAME-1" "TB-FOO-NICKNAME-2" "TB-FOO-NICKNAME-3"))
    (and
     (packagep
      (defpackage "TB-FOO"
        (:nicknames tb-foo-nickname-1 tb-foo-nickname-2 tb-foo-nickname-3)))
     (equal (package-nicknames 'tb-foo)
            '("TB-FOO-NICKNAME-1" "TB-FOO-NICKNAME-2" "TB-FOO-NICKNAME-3"))))))
(define-test sacla-must-package.237 (:tag :sacla)
 (assert-true
  (progn
    (mapc
     #'(lambda (name)
         (when (find-package name)
           (delete-package name)))
     '("A" "B" "C" "D"))
    (and
     (packagep
      (defpackage "A"
        (:nicknames #\B c "D")))
     (null
      (set-difference (package-nicknames 'a)
                      '("B" "C" "D")
                      :test #'string=))))))
(define-test sacla-must-package.238 (:tag :sacla)
 (assert-true
  (progn
    (mapc
     #'(lambda (name)
         (when (find-package name)
           (delete-package name)))
     '("A" "B" "C" "D"))
    (and
     (packagep
      (defpackage "A"
        (:nicknames)
        (:nicknames #\B)
        (:nicknames c "D")))
     (null
      (set-difference (package-nicknames 'a)
                      '("B" "C" "D")
                      :test #'string=))))))
(define-test sacla-must-package.239 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and
     (packagep
      (defpackage "TB-FOO"
        (:use)))
     (null (package-use-list 'tb-foo))))))
(define-test sacla-must-package.240 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and
     (packagep
      (defpackage "TB-FOO"
        (:use cl)))
     (equal (package-use-list 'tb-foo) (list (find-package 'cl)))))))
(define-test sacla-must-package.241 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE" :use nil)
    (and
     (packagep
      (defpackage "TB-FOO"
        (:use cl tb-bar-to-use)))
     (null
      (set-difference (package-use-list 'tb-foo)
                      (mapcar #'find-package '(cl tb-bar-to-use))))))))
(define-test sacla-must-package.242 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE" :use nil)
    (and
     (packagep
      (defpackage "TB-FOO"
        (:use cl)
        (:use)
        (:use tb-bar-to-use)))
     (null
      (set-difference (package-use-list 'tb-foo)
                      (mapcar #'find-package '(cl tb-bar-to-use))))))))
(define-test sacla-must-package.243 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE" :use nil)
    (and
     (packagep
      (defpackage "TB-FOO"
        (:use cl)
        (:use)
        (:use "TB-BAR-TO-USE")))
     (null
      (set-difference (package-use-list 'tb-foo)
                      (mapcar #'find-package '(cl tb-bar-to-use))))))))
(define-test sacla-must-package.244 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (when (find-package "B")
      (mapcan #'delete-package (package-used-by-list "B"))
      (delete-package "B"))
    (make-package "B" :use nil)
    (and
     (packagep
      (defpackage "TB-FOO"
        (:use cl)
        (:use)
        (:use "B")))
     (null
      (set-difference (package-use-list 'tb-foo)
                      (mapcar #'find-package '(cl b))))))))
(define-test sacla-must-package.245 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (when (find-package "B")
      (mapcan #'delete-package (package-used-by-list "B"))
      (delete-package "B"))
    (make-package "B" :use nil)
    (and
     (packagep
      (defpackage "TB-FOO"
        (:use cl)
        (:use)
        (:use #\B)))
     (null
      (set-difference (package-use-list 'tb-foo)
                      (mapcar #'find-package '(cl b))))))))
(define-test sacla-must-package.246 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (when (find-package "B")
      (mapcan #'delete-package (package-used-by-list "B"))
      (delete-package "B"))
    (make-package "B" :use nil)
    (and
     (packagep
      (eval
       `(defpackage ,"TB-FOO"
          (:use cl)
          (:use)
          (:use ,(find-package #\B)))))
     (null
      (set-difference (package-use-list 'tb-foo)
                      (mapcar #'find-package '(cl b))))))))
(define-test sacla-must-package.247 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and
     (packagep
      (defpackage "TB-FOO"
        (:shadow)))
     (null (package-shadowing-symbols 'tb-foo))))))
(define-test sacla-must-package.248 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and
     (packagep
      (defpackage "TB-FOO"
        (:shadow "A")))
     (equal (package-shadowing-symbols 'tb-foo)
            (list (find-symbol "A" 'tb-foo)))))))
(define-test sacla-must-package.249 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and
     (packagep
      (defpackage "TB-FOO"
        (:shadow a #\b "c" "D")))
     (null
      (set-difference (package-shadowing-symbols 'tb-foo)
                      (mapcar #'(lambda (name) (find-symbol name 'tb-foo))
                              '("A" "b" "c" "D"))))))))
(define-test sacla-must-package.250 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and
     (packagep
      (defpackage "TB-FOO"
        (:shadow a)
        (:shadow)
        (:shadow #\b "c" "D"))))
    (null
     (set-difference (package-shadowing-symbols 'tb-foo)
                     (mapcar #'(lambda (name) (find-symbol name 'tb-foo))
                             '("A" "b" "c" "D")))))))
(define-test sacla-must-package.251 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and
     (packagep
      (defpackage "TB-FOO"
        (:shadowing-import-from cl)))))))
(define-test sacla-must-package.252 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and
     (packagep
      (defpackage "TB-FOO"
        (:shadowing-import-from "COMMON-LISP")))
     (null (package-shadowing-symbols 'tb-foo))))))
(define-test sacla-must-package.253 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and
     (packagep
      (defpackage "TB-FOO"
        (:shadowing-import-from "COMMON-LISP" car cdr list)))
     (every #'(lambda (name) (find-symbol name 'tb-foo)) '("CAR" "CDR" "LIST"))
     (null
      (set-difference (package-shadowing-symbols 'tb-foo) '(car cdr list)))))))
(define-test sacla-must-package.254 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and
     (packagep
      (defpackage "TB-FOO"
        (:shadowing-import-from "COMMON-LISP" car cdr)
        (:shadowing-import-from "COMMON-LISP")
        (:shadowing-import-from "COMMON-LISP" list)))
     (every #'(lambda (name) (find-symbol name 'tb-foo)) '("CAR" "CDR" "LIST"))
     (null
      (set-difference (package-shadowing-symbols 'tb-foo) '(car cdr list)))))))
(define-test sacla-must-package.255 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE" :use nil)
    (let ((buz (intern "BUZ" 'tb-bar-to-use)))
      (and
       (packagep
        (defpackage "TB-FOO"
          (:shadowing-import-from "COMMON-LISP" car cdr)
          (:shadowing-import-from tb-bar-to-use "BUZ")))
       (every #'(lambda (name) (find-symbol name 'tb-foo)) '("CAR" "CDR"))
       (null
        (set-difference (package-shadowing-symbols 'tb-foo)
                        (list 'car 'cdr buz))))))))
(define-test sacla-must-package.256 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE" :use nil)
    (let ((buz (intern "BUZ" 'tb-bar-to-use))
          (baz (intern "BAZ" 'tb-bar-to-use)))
      (and
       (packagep
        (defpackage "TB-FOO"
          (:shadowing-import-from "COMMON-LISP" car cdr)
          (:shadowing-import-from tb-bar-to-use "BUZ" "BAZ")))
       (every #'(lambda (name) (find-symbol name 'tb-foo))
              '("CAR" "CDR" "BUZ" "BAZ"))
       (null
        (set-difference (package-shadowing-symbols 'tb-foo)
                        (list 'car 'cdr buz baz))))))))
(define-test sacla-must-package.257 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE" :use nil)
    (let ((buz (intern "BUZ" 'tb-bar-to-use))
          (baz (intern "BAZ" 'tb-bar-to-use)))
      (and
       (packagep
        (defpackage "TB-FOO"
          (:shadow "BOO")
          (:shadowing-import-from "COMMON-LISP" car cdr)
          (:shadowing-import-from tb-bar-to-use "BUZ" "BAZ")))
       (every #'(lambda (name) (find-symbol name 'tb-foo))
              '("CAR" "CDR" "BUZ" "BAZ" "BOO"))
       (null
        (set-difference (package-shadowing-symbols 'tb-foo)
                        (list 'car
                              'cdr
                              buz
                              baz
                              (find-symbol "BOO" 'tb-foo)))))))))
(define-test sacla-must-package.258 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and
     (packagep
      (eval
       `(defpackage ,"TB-FOO"
          (:shadowing-import-from ,(find-package 'cl) "CAR" "CDR"))))
     (every #'(lambda (name) (find-symbol name 'tb-foo)) '("CAR" "CDR"))))))
(define-test sacla-must-package.259 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and
     (packagep
      (eval
       `(defpackage ,"TB-FOO"
          (:import-from ,(find-package 'cl) "CAR" "CDR"))))
     (every #'(lambda (name) (find-symbol name 'tb-foo)) '("CAR" "CDR"))))))
(define-test sacla-must-package.260 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (packagep
     (defpackage "TB-FOO"
       (:import-from cl))))))
(define-test sacla-must-package.261 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and
     (packagep
      (defpackage "TB-FOO"
        (:import-from cl "CAR" "CDR")))
     (every #'(lambda (name) (find-symbol name 'tb-foo)) '("CAR" "CDR"))))))
(define-test sacla-must-package.262 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and
     (packagep
      (defpackage "TB-FOO"
        (:import-from "COMMON-LISP" car cdr list)))
     (every #'(lambda (name) (find-symbol name 'tb-foo))
            '("CAR" "CDR" "LIST"))))))
(define-test sacla-must-package.263 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and
     (packagep
      (defpackage "TB-FOO"
        (:import-from "COMMON-LISP" car cdr)
        (:import-from "COMMON-LISP")
        (:import-from "COMMON-LISP" list)))
     (every #'(lambda (name) (find-symbol name 'tb-foo))
            '("CAR" "CDR" "LIST"))))))
(define-test sacla-must-package.264 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE" :use nil)
    (let ((buz (intern "BUZ" 'tb-bar-to-use)))
      (and
       (packagep
        (defpackage "TB-FOO"
          (:import-from "COMMON-LISP" car cdr)
          (:import-from tb-bar-to-use "BUZ")))
       (every #'(lambda (name) (find-symbol name 'tb-foo)) '("CAR" "CDR"))
       (eq (find-symbol "BUZ" 'tb-foo) buz))))))
(define-test sacla-must-package.265 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE" :use nil)
    (let ((buz (intern "BUZ" 'tb-bar-to-use))
          (baz (intern "BAZ" 'tb-bar-to-use)))
      (and
       (packagep
        (defpackage "TB-FOO"
          (:import-from "COMMON-LISP" car cdr)
          (:import-from tb-bar-to-use "BUZ" "BAZ")))
       (every #'(lambda (name) (find-symbol name 'tb-foo))
              '("CAR" "CDR" "BUZ" "BAZ"))
       (eq (find-symbol "BUZ" 'tb-foo) buz)
       (eq (find-symbol "BAZ" 'tb-foo) baz))))))
(define-test sacla-must-package.266 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (packagep
     (defpackage "TB-FOO"
       (:export))))))
(define-test sacla-must-package.267 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (packagep
     (defpackage "TB-FOO"
       (:export)
       (:export))))))
(define-test sacla-must-package.268 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and
     (packagep
      (defpackage "TB-FOO"
        (:export "A")))
     (eq (cadr (multiple-value-list (find-symbol "A" 'tb-foo))) :external)))))
(define-test sacla-must-package.269 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and
     (packagep
      (defpackage "TB-FOO"
        (:export "A" "B" "C")))
     (every
      #'(lambda (name)
          (eq (cadr (multiple-value-list (find-symbol name 'tb-foo)))
              :external))
      '("A" "B" "C"))))))
(define-test sacla-must-package.270 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and
     (packagep
      (defpackage "TB-FOO"
        (:export "A" "B" "C")))
     (every
      #'(lambda (name)
          (eq (cadr (multiple-value-list (find-symbol name 'tb-foo)))
              :external))
      '("A" "B" "C"))))))
(define-test sacla-must-package.271 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and
     (packagep
      (defpackage "TB-FOO"
        (:export "A")
        (:export "B")
        (:export "C")))
     (every
      #'(lambda (name)
          (eq (cadr (multiple-value-list (find-symbol name 'tb-foo)))
              :external))
      '("A" "B" "C"))))))
(define-test sacla-must-package.272 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and
     (packagep
      (defpackage "TB-FOO"
        (:export "A" "B" "C" "CAR")
        (:use cl)))
     (every
      #'(lambda (name)
          (eq (cadr (multiple-value-list (find-symbol name 'tb-foo)))
              :external))
      '("A" "B" "C" "CAR"))
     (eq (find-symbol "CAR" 'tb-foo) 'car)))))
(define-test sacla-must-package.273 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and
     (packagep
      (defpackage "TB-FOO"
        (:export "A" "B" "C" "CAR")
        (:import-from cl "CAR")))
     (every
      #'(lambda (name)
          (eq (cadr (multiple-value-list (find-symbol name 'tb-foo)))
              :external))
      '("A" "B" "C" "CAR"))
     (eq (find-symbol "CAR" 'tb-foo) 'car)))))
(define-test sacla-must-package.274 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and
     (packagep
      (defpackage "TB-FOO"
        (:export "A" "B" "C" "CAR")
        (:shadowing-import-from cl "CAR")))
     (every
      #'(lambda (name)
          (eq (cadr (multiple-value-list (find-symbol name 'tb-foo)))
              :external))
      '("A" "B" "C" "CAR"))
     (eq (find-symbol "CAR" 'tb-foo) 'car)))))
(define-test sacla-must-package.275 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE" :use nil)
    (let ((buz (intern "BUZ" 'tb-bar-to-use)))
      (and
       (packagep
        (defpackage "TB-FOO"
          (:export "A" "B" "C" "CAR" "CDR" "BUZ")
          (:use tb-bar-to-use)
          (:import-from cl "CDR")
          (:shadowing-import-from cl "CAR")))
       (every
        #'(lambda (name)
            (eq (cadr (multiple-value-list (find-symbol name 'tb-foo)))
                :external))
        '("A" "B" "C" "CAR" "CDR" "BUZ"))
       (eq (find-symbol "CAR" 'tb-foo) 'car)
       (eq (find-symbol "CDR" 'tb-foo) 'cdr)
       (eq (find-symbol "BUZ" 'tb-bar-to-use) buz))))))
(define-test sacla-must-package.276 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (packagep
     (defpackage "TB-FOO"
       (:intern))))))
(define-test sacla-must-package.277 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (packagep
     (defpackage "TB-FOO"
       (:intern)
       (:intern))))))
(define-test sacla-must-package.278 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and
     (packagep
      (defpackage "TB-FOO"
        (:intern "A")))
     (eq (cadr (multiple-value-list (find-symbol "A" 'tb-foo))) :internal)))))
(define-test sacla-must-package.279 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and
     (packagep
      (defpackage "TB-FOO"
        (:intern "A" "B" "C")))
     (every
      #'(lambda (name)
          (eq (cadr (multiple-value-list (find-symbol name 'tb-foo)))
              :internal))
      '("A" "B" "C"))))))
(define-test sacla-must-package.280 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and
     (packagep
      (defpackage "TB-FOO"
        (:intern "A" "B" "C")))
     (every
      #'(lambda (name)
          (eq (cadr (multiple-value-list (find-symbol name 'tb-foo)))
              :internal))
      '("A" "B" "C"))))))
(define-test sacla-must-package.281 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and
     (packagep
      (defpackage "TB-FOO"
        (:intern "A")
        (:intern "B")
        (:intern "C")))
     (every
      #'(lambda (name)
          (eq (cadr (multiple-value-list (find-symbol name 'tb-foo)))
              :internal))
      '("A" "B" "C"))))))
(define-test sacla-must-package.282 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and
     (packagep
      (defpackage "TB-FOO"
        (:intern "A" "B" "C" "CAR")
        (:use cl)))
     (every
      #'(lambda (name)
          (eq (cadr (multiple-value-list (find-symbol name 'tb-foo)))
              :internal))
      '("A" "B" "C"))
     (equal (multiple-value-list (find-symbol "CAR" 'tb-foo))
            '(car :inherited))))))
(define-test sacla-must-package.283 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and
     (packagep
      (defpackage "TB-FOO"
        (:size 10)))))))
(define-test sacla-must-package.284 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and
     (packagep
      (defpackage "TB-FOO"
        (:size 0)))))))
(define-test sacla-must-package.285 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (and
     (packagep
      (defpackage "TB-FOO"
        (:size 1000)))))))
(define-test sacla-must-package.286 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE" :use nil)
    (let ((buz (intern "BUZ" 'tb-bar-to-use)))
      (export buz 'tb-bar-to-use)
      (and
       (packagep
        (defpackage "TB-FOO"
          (:size 10)
          (:shadow "SHADOW1" "SHADOW2")
          (:shadowing-import-from cl "CAR" "CDR")
          (:use tb-bar-to-use)
          (:import-from keyword "TEST")
          (:intern "S0" "S1")
          (:nicknames "TB-FOO-NICKNAME-0" "TB-FOO-NICKNAME-1"
                      "TB-FOO-NICKNAME-2")
          (:export "SHADOW1" "CAR")))
       (equal (multiple-value-list (find-symbol "BUZ" 'tb-foo-nickname-0))
              (list buz :inherited))
       (eq
        (cadr (multiple-value-list (find-symbol "SHADOW1" 'tb-foo-nickname-2)))
        :external)
       (eq
        (cadr (multiple-value-list (find-symbol "SHADOW2" 'tb-foo-nickname-2)))
        :internal)
       (equal (multiple-value-list (find-symbol "CAR" 'tb-foo-nickname-2))
              (list 'car :external))
       (equal (multiple-value-list (find-symbol "CDR" 'tb-foo-nickname-2))
              (list 'cdr :internal))
       (equal (multiple-value-list (find-symbol "TEST" 'tb-foo-nickname-2))
              (list :test :internal))
       (eq (cadr (multiple-value-list (find-symbol "S0" 'tb-foo-nickname-2)))
           :internal)
       (eq (cadr (multiple-value-list (find-symbol "S1" 'tb-foo-nickname-2)))
           :internal))))))
(define-test sacla-must-package.287 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (make-package "TB-BAR-TO-USE" :use nil)
    (let ((buz (intern "BUZ" 'tb-bar-to-use)))
      (export buz 'tb-bar-to-use)
      (and
       (packagep
        (defpackage "TB-FOO"
          (:export "SHADOW1")
          (:size 10)
          (:nicknames "TB-FOO-NICKNAME-1" "TB-FOO-NICKNAME-2")
          (:shadow "SHADOW1")
          (:shadowing-import-from cl "CAR")
          (:intern "S1")
          (:shadowing-import-from cl)
          (:use tb-bar-to-use)
          (:nicknames "TB-FOO-NICKNAME-0")
          (:shadowing-import-from cl "CDR")
          (:shadow "SHADOW2")
          (:import-from keyword "TEST")
          (:intern "S0")
          (:nicknames)
          (:export "CAR")))
       (equal (multiple-value-list (find-symbol "BUZ" 'tb-foo-nickname-0))
              (list buz :inherited))
       (eq
        (cadr (multiple-value-list (find-symbol "SHADOW1" 'tb-foo-nickname-2)))
        :external)
       (eq
        (cadr (multiple-value-list (find-symbol "SHADOW2" 'tb-foo-nickname-2)))
        :internal)
       (equal (multiple-value-list (find-symbol "CAR" 'tb-foo-nickname-2))
              (list 'car :external))
       (equal (multiple-value-list (find-symbol "CDR" 'tb-foo-nickname-2))
              (list 'cdr :internal))
       (equal (multiple-value-list (find-symbol "TEST" 'tb-foo-nickname-2))
              (list :test :internal))
       (eq (cadr (multiple-value-list (find-symbol "S0" 'tb-foo-nickname-2)))
           :internal)
       (eq (cadr (multiple-value-list (find-symbol "S1" 'tb-foo-nickname-2)))
           :internal))))))
(define-test sacla-must-package.288 (:tag :sacla)
 (assert-true
  (with-package-iterator (get "CL" :external)
    (multiple-value-bind (more symbol status pkg)
        (get)
      (declare (ignore more))
      (and (symbolp symbol)
           (eq status :external)
           (eq pkg (find-package 'cl)))))))
(define-test sacla-must-package.289 (:tag :sacla)
 (assert-true
  (with-package-iterator (get 'cl :external)
    (multiple-value-bind (more symbol status pkg)
        (get)
      (declare (ignore more))
      (and (symbolp symbol)
           (eq status :external)
           (eq pkg (find-package 'cl)))))))
(define-test sacla-must-package.290 (:tag :sacla)
 (assert-true
  (with-package-iterator (get (find-package 'cl) :external)
    (multiple-value-bind (more symbol status pkg)
        (get)
      (declare (ignore more))
      (and (symbolp symbol)
           (eq status :external)
           (eq pkg (find-package 'cl)))))))
(define-test sacla-must-package.291 (:tag :sacla)
 (assert-true
  (with-package-iterator (get '(cl) :external)
    (multiple-value-bind (more symbol status pkg)
        (get)
      (declare (ignore more))
      (and (symbolp symbol)
           (eq status :external)
           (eq pkg (find-package 'cl)))))))
(define-test sacla-must-package.292 (:tag :sacla)
 (assert-true
  (with-package-iterator (get (list "CL") :external)
    (multiple-value-bind (more symbol status pkg)
        (get)
      (declare (ignore more))
      (and (symbolp symbol)
           (eq status :external)
           (eq pkg (find-package 'cl)))))))
(define-test sacla-must-package.293 (:tag :sacla)
 (assert-true
  (with-package-iterator (get (list (find-package "COMMON-LISP")) :external)
    (multiple-value-bind (more symbol status pkg)
        (get)
      (declare (ignore more))
      (and (symbolp symbol)
           (eq status :external)
           (eq pkg (find-package 'cl)))))))
(define-test sacla-must-package.294 (:tag :sacla)
 (assert-true
  (with-package-iterator (get 'cl :external :internal :inherited)
    (multiple-value-bind (more symbol status pkg)
        (get)
      (declare (ignore more))
      (and (symbolp symbol)
           (member status '(:external :internal :inherited))
           (eq pkg (find-package 'cl)))))))
(define-test sacla-must-package.295 (:tag :sacla)
 (assert-true
  (with-package-iterator (get (list 'cl) :internal)
    (multiple-value-bind (more symbol status pkg)
        (get)
      (or (not more)
          (and (symbolp symbol)
               (eq status :internal)
               (eq pkg (find-package 'cl))))))))
(define-test sacla-must-package.296 (:tag :sacla)
 (assert-true
  (with-package-iterator (get (list 'cl) :inherited)
    (multiple-value-bind (more symbol status pkg)
        (get)
      (or (not more)
          (and (symbolp symbol)
               (eq status :inherited)
               (eq pkg (find-package 'cl))))))))
(define-test sacla-must-package.297 (:tag :sacla)
 (assert-true
  (progn
    'skipped)))
(define-test sacla-must-package.298 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((package (make-package "TB-FOO" :use nil)) list)
      (with-package-iterator (get package :internal)
        (and
         (loop
            (multiple-value-bind (more symbol status pkg)
                (get)
              (declare (ignore status pkg))
              (unless more
                (return t))
              (push symbol list)))
         (null list)))))))
(define-test sacla-must-package.299 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((package (make-package "TB-FOO" :use nil)))
      (dolist (name '(a b c d e f g "S1" "S2" "ss")) (intern name package))
      (with-package-iterator (get package :internal)
        (loop
           (multiple-value-bind (more symbol status pkg)
               (get)
             (unless more
               (return t))
             (unless
                 (and (eq status :internal)
                      (eq pkg package)
                      (eq symbol (find-symbol (string symbol) pkg)))
               (return nil)))))))))
(define-test sacla-must-package.300 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package #\a)
      (delete-package #\a))
    (let ((package (make-package #\a :use nil)))
      (dolist (name '(a b c d e f g "S1" "S2" "ss")) (intern name package))
      (with-package-iterator (get #\a :internal)
        (loop
           (multiple-value-bind (more symbol status pkg)
               (get)
             (unless more
               (return t))
             (unless
                 (and (eq status :internal)
                      (eq pkg package)
                      (eq symbol (find-symbol (string symbol) pkg)))
               (return nil)))))))))
(define-test sacla-must-package.301 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package #\a)
      (delete-package #\a))
    (let ((package (make-package #\a :use nil)))
      (dolist (name '(a b c d e f g "S1" "S2" "ss")) (intern name package))
      (with-package-iterator (get (list #\a) :internal)
        (loop
           (multiple-value-bind (more symbol status pkg)
               (get)
             (unless more
               (return t))
             (unless
                 (and (eq status :internal)
                      (eq pkg package)
                      (eq symbol (find-symbol (string symbol) pkg)))
               (return nil)))))))))
(define-test sacla-must-package.302 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (when (find-package "TB-BAR-TO-USE")
      (mapcan #'delete-package (package-used-by-list "TB-BAR-TO-USE"))
      (delete-package "TB-BAR-TO-USE"))
    (let* ((package (make-package "TB-BAR-TO-USE" :use nil))
           (package-1 (make-package "TB-FOO" :use (list package)))
           (symbol-list nil))
      (export (intern "S" package) package)
      (shadow '("S") package-1)
      (with-package-iterator (get package-1 :internal :external :inherited)
        (loop
           (multiple-value-bind (more symbol status pkg)
               (get)
             (declare (ignore status pkg))
             (unless more
               (return t))
             (push symbol symbol-list))))
      (not (member (intern "S" package) symbol-list))))))
(define-test sacla-must-package.303 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let* ((package (make-package "TB-FOO" :use nil)) (symbol-list nil))
      (with-package-iterator (get package :internal :external)
        (loop
           (multiple-value-bind (more symbol status pkg)
               (get)
             (declare (ignore status pkg))
             (unless more
               (return t))
             (push symbol symbol-list))))
      (null symbol-list)))))
(define-test sacla-must-package.304 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let* ((package (make-package "TB-FOO" :use nil))
           (symbol-list '(a b c d car cdr i lisp))
           (list nil))
      (dolist (symbol symbol-list) (shadowing-import symbol package))
      (with-package-iterator (get package :internal)
        (loop
           (multiple-value-bind (more symbol status pkg)
               (get)
             (declare (ignore status pkg))
             (unless more
               (return t))
             (push symbol list))))
      (null (set-difference symbol-list list))))))
(define-test sacla-must-package.305 (:tag :sacla)
 (assert-true
  (with-package-iterator (get 'cl :external)
    (loop
       (multiple-value-bind (more symbol status package)
           (get)
         (unless more
           (return t))
         (unless
             (and (eq status :external)
                  (eq package (find-package 'cl))
                  (eq symbol (find-symbol (symbol-name symbol) 'cl-user)))
           (return nil)))))))
(define-test sacla-must-package.306 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let* ((package (make-package "TB-FOO" :use 'cl)))
      (shadow '("CAR") package)
      (with-package-iterator (get package :external :inherited :internal)
        (loop
           (multiple-value-bind (more symbol status pkg)
               (get)
             (declare (ignore pkg status))
             (unless more
               (return t))
             (when (eq symbol 'car)
               (return nil)))))))))
(define-test sacla-must-package.307 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let* ((*package* (make-package "TB-FOO" :use nil))
           (names '("BLACK" "RED" "WHITE" "YELLOW" "VIOLET" "BROWN" "BLUE"))
           list)
      (mapc #'intern names)
      (export
       (mapcar #'find-symbol
               (mapcan
                #'(lambda (name)
                    (when (= (length name) 5)
                      (list name)))
                names)))
      (with-package-iterator (get *package* :external :inherited :internal)
        (loop
           (multiple-value-bind (more symbol status pkg)
               (get)
             (declare (ignore pkg))
             (unless more
               (return))
             (push (symbol-name symbol) (getf list status)))))
      (and
       (null
        (set-difference (getf list :external)
                        '("BLACK" "WHITE" "BROWN")
                        :test #'string=))
       (null
        (set-difference (getf list :internal)
                        '("RED" "YELLOW" "VIOLET" "BLUE")
                        :test #'string=))
       (null (getf list :inherited)))))))
(define-test sacla-must-package.308 (:tag :sacla)
 (assert-true
  (flet ((test-package-iterator (package)
           (unless (packagep package)
             (setq package (find-package package)))
           (let ((all-entries 'nil) (generated-entries 'nil))
             (do-symbols (x package)
               (multiple-value-bind (symbol accessibility)
                   (find-symbol (symbol-name x) package)
                 (push (list symbol accessibility) all-entries)))
             (with-package-iterator
                 (generator-fn package :internal :external :inherited)
               (loop
                  (multiple-value-bind (more? symbol accessibility pkg)
                      (generator-fn)
                    (declare (ignore pkg))
                    (unless more?
                      (return))
                    (let ((l
                           (multiple-value-list
                            (find-symbol (symbol-name symbol) package))))
                      (unless (equal l (list symbol accessibility))
                        (error "Symbol ~S not found as ~S in package ~A [~S]"
                               symbol
                               accessibility
                               (package-name package)
                               l))
                      (push l generated-entries)))))
             (unless
                 (and (subsetp all-entries generated-entries :test #'equal)
                      (subsetp generated-entries all-entries :test #'equal))
               (error
                "Generated entries and Do-Symbols entries don't correspond"))
             t)))
    (every #'test-package-iterator '("CL" "CL-USER" "KEYWORD")))))
(define-test sacla-must-package.309 (:tag :sacla)
 (assert-true (null (do-symbols (symbol) (declare (ignore symbol))))))
(define-test sacla-must-package.310 (:tag :sacla)
 (assert-true (null (do-symbols (symbol *package*) (declare (ignore symbol))))))
(define-test sacla-must-package.311 (:tag :sacla)
 (assert-true (null (do-external-symbols (symbol) (declare (ignore symbol))))))
(define-test sacla-must-package.312 (:tag :sacla)
 (assert-true
  (null (do-external-symbols (symbol *package*) (declare (ignore symbol))))))
(define-test sacla-must-package.313 (:tag :sacla)
 (assert-true (null (do-all-symbols (symbol) (declare (ignore symbol))))))
(define-test sacla-must-package.314 (:tag :sacla)
 (assert-true (do-symbols (symbol *package* (null symbol)))))
(define-test sacla-must-package.315 (:tag :sacla)
 (assert-true (do-external-symbols (symbol *package* (null symbol)))))
(define-test sacla-must-package.316 (:tag :sacla)
 (assert-true (do-all-symbols (symbol (null symbol)))))
(define-test sacla-must-package.317 (:tag :sacla)
 (assert-true
  (do-symbols (symbol 'cl nil) (declare (ignore symbol)) (return t))))
(define-test sacla-must-package.318 (:tag :sacla)
 (assert-true
  (do-external-symbols (symbol 'cl nil) (declare (ignore symbol)) (return t))))
(define-test sacla-must-package.319 (:tag :sacla)
 (assert-true
  (do-all-symbols (symbol nil) (declare (ignore symbol)) (return t))))
(define-test sacla-must-package.320 (:tag :sacla)
 (assert-true
  (do-symbols (symbol 'cl nil)
    (go start)
   found
    (return t)
   start
    (when (eq symbol 'car)
      (go found)))))
(define-test sacla-must-package.321 (:tag :sacla)
 (assert-true
  (do-external-symbols (symbol 'cl nil)
    (go start)
   found
    (return t)
   start
    (when (eq symbol 'car)
      (go found)))))
(define-test sacla-must-package.322 (:tag :sacla)
 (assert-true
  (do-all-symbols (symbol nil)
    (go start)
   found
    (return t)
   start
    (when (eq symbol 'car)
      (go found)))))
(define-test sacla-must-package.323 (:tag :sacla)
 (assert-true
  (let ((i 0) (list nil) (*package* (find-package "COMMON-LISP-USER")))
    (do-symbols (symbol)
      (push symbol list)
      (incf i)
      (when (= i 10)
        (return)))
    (every #'symbolp list))))
(define-test sacla-must-package.324 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use nil))
          (name-list '("A" "B" "DOG" "CAT" "giraffe" "hippo" "wolf"))
          (list))
      (export (mapcar #'intern name-list))
      (null
       (set-difference
        (do-symbols (symbol *package* list) (pushnew symbol list))
        (mapcar #'find-symbol name-list)))))))
(define-test sacla-must-package.325 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use nil)) list)
      (do-symbols (symbol *package*) (push symbol list))
      (null list)))))
(define-test sacla-must-package.326 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use nil)) list)
      (do-symbols (symbol) (push symbol list))
      (null list)))))
(define-test sacla-must-package.327 (:tag :sacla)
 (assert-true
  (do-symbols (symbol 'cl t)
    (unless (eq symbol (find-symbol (symbol-name symbol) 'cl))
      (return nil)))))
(define-test sacla-must-package.328 (:tag :sacla)
 (assert-true
  (do-symbols (symbol 'keyword t)
    (unless
        (equal
         (multiple-value-list (find-symbol (symbol-name symbol) 'keyword))
         (list symbol :external))
      (return nil)))))
(define-test sacla-must-package.329 (:tag :sacla)
 (assert-true
  (let (list1 list2)
    (and (do-external-symbols (symbol 'keyword t) (push symbol list1))
         (do-symbols (symbol 'keyword t) (push symbol list2))
         (null (set-difference list1 list2))))))
(define-test sacla-must-package.330 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use nil)) list)
      (do-external-symbols (symbol *package*) (push symbol list))
      (null list)))))
(define-test sacla-must-package.331 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use nil)) list)
      (do-external-symbols (symbol) (push symbol list))
      (null list)))))
(define-test sacla-must-package.332 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use nil))
          (name-list '("A" "B" "DOG" "CAT" "giraffe" "hippo" "wolf"))
          (list))
      (export (mapcar #'intern name-list))
      (null
       (set-difference
        (do-external-symbols (symbol *package* list) (pushnew symbol list))
        (mapcar #'find-symbol name-list)))))))
(define-test sacla-must-package.333 (:tag :sacla)
 (assert-true
  (progn
    (when (find-package "TB-FOO")
      (delete-package "TB-FOO"))
    (let ((*package* (make-package "TB-FOO" :use nil))
          (name-list '("A" "B" "DOG" "CAT" "giraffe" "hippo" "wolf"))
          (list))
      (mapcar #'intern name-list)
      (null
       (do-external-symbols (symbol *package* list) (pushnew symbol list)))))))
(define-test sacla-must-package.334 (:tag :sacla)
 (assert-true
  (let ((i 0) (list nil))
    (do-all-symbols (symbol)
      (push symbol list)
      (incf i)
      (when (= i 10)
        (return)))
    (every #'symbolp list))))
(define-test sacla-must-package.335 (:tag :sacla)
 (assert-true
  (let ((list nil))
    (do-all-symbols (symbol) (push symbol list))
    (with-package-iterator (get (list-all-packages) :external :internal)
      (loop
         (multiple-value-bind (more symbol status package)
             (get)
           (declare (ignore status package))
           (unless more
             (return t))
           (unless (member symbol list)
             (return nil))))))))

