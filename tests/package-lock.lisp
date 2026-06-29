;;; Regression tests for package-locked-error signaling.
;;;
;;; These tests exist as a baseline before refactoring the
;;; package-lock checks into a shared helper.  They verify that
;;; package-locked-error is signaled at each call site we expect to
;;; check the lock; restart behavior is tested separately.

(defpackage :package-lock-tests
  (:use :cl :lisp-unit))

(in-package "PACKAGE-LOCK-TESTS")

(defpackage :test-locked-package
  (:use :cl))

(defmacro with-definition-locked ((package) &body body)
  "Run BODY with PACKAGE's definition-lock enabled and namespace-lock
   disabled, so failures from BODY can be attributed unambiguously to
   the definition lock."
  `(let ((p ,package))
     (setf (ext:package-definition-lock p) t
           (ext:package-lock p) nil)
     (assert-true (ext:package-definition-lock p))
     (assert-false (ext:package-lock p))
     (unwind-protect (progn ,@body)
       (setf (ext:package-definition-lock p) nil
             (ext:package-lock p) nil))))

(defmacro with-namespace-locked ((package) &body body)
  "Run BODY with PACKAGE's namespace-lock enabled and definition-lock
   disabled, so failures from BODY can be attributed unambiguously to
   the namespace lock."
  `(let ((p ,package))
     (setf (ext:package-definition-lock p) nil
           (ext:package-lock p) t)
     (assert-false (ext:package-definition-lock p))
     (assert-true (ext:package-lock p))
     (unwind-protect (progn ,@body)
       (setf (ext:package-definition-lock p) nil
             (ext:package-lock p) nil))))


;;; ---- Definition-lock tests ----

(define-test package-locked.defmacro
  (:tag :issues)
  (with-definition-locked ((find-package :test-locked-package))
    (assert-error 'lisp::package-locked-error
                  (eval '(defmacro test-locked-package::a-macro (x)
                          `(list ,x))))))

(define-test package-locked.defun
  (:tag :issues)
  (with-definition-locked ((find-package :test-locked-package))
    (assert-error 'lisp::package-locked-error
                  (eval '(defun test-locked-package::a-fn (x) x)))))

(define-test package-locked.deftype
  (:tag :issues)
  (with-definition-locked ((find-package :test-locked-package))
    (assert-error 'lisp::package-locked-error
                  (eval '(deftype test-locked-package::a-type ()
                          'integer)))))

(define-test package-locked.defstruct
  (:tag :issues)
  (with-definition-locked ((find-package :test-locked-package))
    (assert-error 'lisp::package-locked-error
                  (eval '(defstruct test-locked-package::a-struct
                          slot-1 slot-2)))))


;;; ---- Namespace-lock tests ----

(define-test package-locked.unintern
  (:tag :issues)
  (let ((sym (intern "TO-BE-UNINTERNED"
                     (find-package :test-locked-package))))
    (with-namespace-locked ((find-package :test-locked-package))
      (assert-error 'lisp::package-locked-error
                    (unintern sym (find-package :test-locked-package))))))

(define-test package-locked.unexport
  (:tag :issues)
  (let* ((p (find-package :test-locked-package))
         (sym (intern "TO-BE-UNEXPORTED" p)))
    (export sym p)
    (with-namespace-locked (p)
      (assert-error 'lisp::package-locked-error
                    (unexport sym p)))))

