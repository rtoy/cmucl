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

(let* ((p (find-package :test-locked-package))
       (sym (intern "TOPLEVEL-PROBE" p)))
  (setf (ext:package-lock p) t)
  (format t "~&TOPLEVEL: package-lock=~S enable=~S~%"
          (ext:package-lock p) lisp::*enable-package-locked-errors*)
  (handler-case
      (let ((result (unintern sym p)))
        (format t "~&TOPLEVEL: unintern returned ~S (no error)~%" result))
    (lisp::package-locked-error (c)
      (format t "~&TOPLEVEL: caught error ~A~%" c))
    (error (c)
      (format t "~&TOPLEVEL: caught other error ~A (~A)~%" c (type-of c))))
  (setf (ext:package-lock p) nil))

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

(define-test package-lock-debug
  (:tag :issues)
  (let* ((p (find-package :test-locked-package))
         (sym (intern "DEBUG-SYM" p)))
    (setf (ext:package-lock p) t)
    (format t "~&Just before unintern:~%")
    (format t "  package-lock: ~S~%" (ext:package-lock p))
    (format t "  *enable-package-locked-errors*: ~S~%"
            lisp::*enable-package-locked-errors*)
    (format t "  *package*: ~S~%" *package*)
    (format t "  package of unintern fn: ~S~%"
            (symbol-package 'unintern))
    (handler-case
        (let ((result (unintern sym p)))
          (format t "  unintern returned: ~S~%" result))
      (lisp::package-locked-error (c)
        (format t "  GOT package-locked-error: ~A~%" c))
      (error (c)
        (format t "  got OTHER error: ~A~%" c)))
    (setf (ext:package-lock p) nil)
    (assert-true t)))

;; At end of file:
(format *error-output* "~&~%==== TOPLEVEL PROBE ====~%")
(force-output *error-output*)
(let* ((p (find-package :test-locked-package))
       (sym (intern "TOPLEVEL-PROBE" p)))
  (setf (ext:package-lock p) t)
  (format *error-output* "package-lock=~S enable=~S~%"
          (ext:package-lock p) lisp::*enable-package-locked-errors*)
  (force-output *error-output*)
  (handler-case
      (let ((result (unintern sym p)))
        (format *error-output* "unintern returned ~S (NO ERROR!)~%" result))
    (lisp::package-locked-error (c)
      (format *error-output* "GOT package-locked-error: ~A~%" c))
    (error (c)
      (format *error-output* "got OTHER error: ~A (~A)~%" c (type-of c))))
  (force-output *error-output*)
  (setf (ext:package-lock p) nil))
(format *error-output* "==== END PROBE ====~%~%")
(force-output *error-output*)
