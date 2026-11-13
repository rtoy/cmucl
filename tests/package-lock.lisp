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

;;; ---- Compile-time lock check for defmacro (issue #499) ----
;;;
;;; The load-time check in %%defmacro fires when a fasl is loaded,
;;; but do-macro-compile-time also mutates globaldb at compile time.
;;; Verify that the compile-time path also signals and that restarts
;;; still reach a handler.

(define-test package-locked.defmacro-compile-time.signals
  (:tag :issues)
  (with-definition-locked ((find-package :test-locked-package))
    (ext:with-temporary-file (tmp :prefix "pkg-lock-cmp-")
      (with-open-file (s tmp :direction :output :if-exists :supersede)
        (format s "(in-package :test-locked-package)~%")
        (format s "(defmacro compile-time-macro (x) `(list ,x))~%"))
      (assert-error 'lisp::package-locked-error
                    (compile-file tmp :output-file nil
                                      :print nil :verbose nil)))))

(define-test package-locked.defmacro-compile-time.no-mutation-when-declined
  (:tag :issues)
  ;; When the error is raised at compile time and no restart is
  ;; invoked, do-macro-compile-time must not have mutated globaldb
  ;; for the locked-package symbol.
  (let ((name (intern "NOT-YET-A-MACRO"
                      (find-package :test-locked-package))))
    ;; Precondition: name has no function/macro binding.
    (assert-false (c::info function kind name))
    (with-definition-locked ((find-package :test-locked-package))
      (handler-case
          (eval `(defmacro ,name (x) `(list ,x)))
        (lisp::package-locked-error () nil)))
    ;; After the error (no restart invoked), nothing should have
    ;; been recorded for NAME.
    (assert-false (c::info function kind name))
    (assert-false (macro-function name))))

(define-test package-locked.defmacro-compile-time.continue-restart
  (:tag :issues)
  ;; The CONTINUE restart must be visible from the compile-time
  ;; error and, when invoked, must allow the definition to proceed.
  (with-definition-locked ((find-package :test-locked-package))
    (handler-bind
        ((lisp::package-locked-error
           (lambda (c)
             (declare (ignore c))
             (let ((restart (find-restart 'continue)))
               (assert-true restart)
               (invoke-restart restart)))))
      (eval '(defmacro test-locked-package::continued-macro (x)
              `(list ,x))))
    (assert-true (macro-function 'test-locked-package::continued-macro))
    (assert-equal '(list 42)
                  (macroexpand-1 '(test-locked-package::continued-macro 42)))))

(define-test package-locked.defmacro-compile-time.unlock-package-restart
  (:tag :issues)
  ;; The UNLOCK-PACKAGE restart must be visible from the compile-time
  ;; error and, when invoked, must disable the lock and allow the
  ;; definition to proceed.
  (let ((p (find-package :test-locked-package)))
    (setf (ext:package-definition-lock p) t
          (ext:package-lock p) nil)
    (unwind-protect
         (progn
           (handler-bind
               ((lisp::package-locked-error
                  (lambda (c)
                    (declare (ignore c))
                    (let ((restart (find-restart 'lisp::unlock-package)))
                      (assert-true restart)
                      (invoke-restart restart)))))
             (eval '(defmacro test-locked-package::unlocked-macro (x)
                     `(list ,x))))
           (assert-true (macro-function 'test-locked-package::unlocked-macro))
           (assert-false (ext:package-definition-lock p)))
      (setf (ext:package-definition-lock p) nil
            (ext:package-lock p) nil))))

(define-test package-locked.defmacro-compile-time.not-a-function-redefinition
  (:tag :issues)
  ;; do-macro-compile-time's :function branch calls undefine-function-name
  ;; and remhashes *free-functions*.  Verify those side effects do NOT
  ;; happen when the lock check aborts first.
  (let ((name (intern "WAS-A-FUNCTION"
                      (find-package :test-locked-package))))
    ;; Set up: name is currently a function.
    (setf (fdefinition name) (lambda (x) x))
    (assert-equal :function (c::info function kind name))
    (with-definition-locked ((find-package :test-locked-package))
      (handler-case
          (eval `(defmacro ,name (x) `(list ,x)))
        (lisp::package-locked-error () nil)))
    ;; After the declined error, name should still be a function,
    ;; not undefined or a macro.
    (assert-equal :function (c::info function kind name))
    (assert-true (fboundp name))
    (assert-false (macro-function name))
    (fmakunbound name)))
