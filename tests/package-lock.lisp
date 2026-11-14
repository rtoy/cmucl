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


;;; ---- Compile-time lock check for defun ----
;;;
;;; The load-time check in REDEFINING-FUNCTION (via
;;; *SETF-FDEFINITION-HOOK*) fires when a fasl is loaded, but the
;;; %DEFUN IR1 translator also mutates globaldb at compile time (via
;;; *FREE-FUNCTIONS*, INLINE-EXPANSION, and LEAF-TYPE).  Verify that
;;; the compile-time path also signals and that restarts still reach
;;; a handler.
;;;
;;; Use COMPILE (not EVAL) to reach the IR1 translator: EVAL takes an
;;; interpreted path through C::%DEFUN's function binding and bypasses
;;; the translator.  COMPILE on an anonymous lambda compiles the body
;;; through the IR1 machinery, so the inner DEFUN hits the compile-
;;; time check.

(define-test package-locked.defun-compile-time.signals
  (:tag :issues)
  (with-definition-locked ((find-package :test-locked-package))
    (ext:with-temporary-file (tmp :prefix "pkg-lock-cmp-defun-")
      (with-open-file (s tmp :direction :output :if-exists :supersede)
        (format s "(in-package :test-locked-package)~%")
        (format s "(defun compile-time-fn (x) x)~%"))
      (assert-error 'lisp::package-locked-error
                    (compile-file tmp :output-file nil
                                      :print nil :verbose nil)))))

(define-test package-locked.defun-compile-time.no-mutation-when-declined
  (:tag :issues)
  ;; When the error is raised at compile time and no restart is
  ;; invoked, the %DEFUN translator must not have mutated globaldb
  ;; for the locked-package symbol.
  (let ((name (intern "NOT-YET-A-FN"
                      (find-package :test-locked-package))))
    ;; Precondition: name has no function binding or inline info.
    (assert-false (c::info function kind name))
    (assert-false (c::info function inline-expansion name))
    (with-definition-locked ((find-package :test-locked-package))
      (handler-case
          (compile nil `(lambda () (defun ,name (x) x)))
        (lisp::package-locked-error () nil)))
    ;; After the declined error, nothing should have been recorded.
    (assert-false (c::info function kind name))
    (assert-false (fboundp name))
    (assert-false (c::info function inline-expansion name))))

(define-test package-locked.defun-compile-time.continue-restart
  (:tag :issues)
  ;; The CONTINUE restart must be visible from the compile-time
  ;; error.  Invoking it lets COMPILE produce a thunk; we don't
  ;; call the thunk, so no load-time effects happen.
  (let ((name (intern "COMPILE-TIME-CONTINUE-FN"
                      (find-package :test-locked-package))))
    (with-definition-locked ((find-package :test-locked-package))
      (handler-bind
          ((lisp::package-locked-error
             (lambda (c)
               (declare (ignore c))
               (let ((restart (find-restart 'continue)))
                 (assert-true restart)
                 (invoke-restart restart)))))
        (compile nil `(lambda () (defun ,name (x) x)))))))

(define-test package-locked.defun-compile-time.unlock-package-restart
  (:tag :issues)
  ;; The UNLOCK-PACKAGE restart must be visible from the compile-time
  ;; error and, when invoked, must disable the lock.
  (let ((p (find-package :test-locked-package))
        (name (intern "COMPILE-TIME-UNLOCK-FN"
                      (find-package :test-locked-package))))
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
             (compile nil `(lambda () (defun ,name (x) x))))
           (assert-false (ext:package-definition-lock p)))
      (setf (ext:package-definition-lock p) nil
            (ext:package-lock p) nil))))

(define-test package-locked.defun-compile-time.setf-function-name
  (:tag :issues)
  ;; DEFUN of (SETF foo) should also check the lock, via
  ;; VALID-FUNCTION-NAME-P's block-name extraction.
  (with-definition-locked ((find-package :test-locked-package))
    (assert-error 'lisp::package-locked-error
                  (compile nil
                           '(lambda ()
                             (defun (setf test-locked-package::setfable) (v x)
                               (declare (ignore v x))
                               nil))))))

(define-test package-locked.defun-compile-time.pcl-method-names-pass
  (:tag :issues)
  ;; PCL-generated compound names like (PCL::FAST-METHOD ...) must
  ;; NOT trip the lock, mirroring REDEFINING-FUNCTION's load-time
  ;; filter.
  (with-definition-locked ((find-package :test-locked-package))
    (handler-case
        (compile nil
                 '(lambda ()
                   (defun (pcl::fast-method test-locked-package::m (t))
                       (&rest args)
                     (declare (ignore args))
                     nil)))
      (lisp::package-locked-error ()
        (assert-true nil "pcl::fast-method name tripped the lock"))
      ;; Any other error (e.g. from the artificial name shape) is fine
      ;; for the purpose of this test.
      (error () nil))))
