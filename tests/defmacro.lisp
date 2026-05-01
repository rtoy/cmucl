;;; Tests for defmacro documention and location info

(defpackage :defmacro-tests
  (:use :cl :lisp-unit))

(in-package "DEFMACRO-TESTS")

(defpackage :test-locked-package
  (:use :cl))

(define-test issue.499.defmacro-signals-locked-package-error
  (:tag :issues)
  (setf (ext:package-definition-lock (find-package :test-locked-package))
	t)
  ;; Without a restart-handler, defining a macro in a locked package
  ;; must signal a package-locked-error.
  (assert-error 'lisp::package-locked-error
		(eval '(defmacro test-locked-package::foo (x)
			`(list ,x)))))

(define-test issue.499.defmacro-continue-restart
  (:tag :issues)
  (setf (ext:package-definition-lock (find-package :test-locked-package))
	t)
  ;; The continue restart lets the macro definition proceed.
  (handler-bind
      ((lisp::package-locked-error
	 #'(lambda (c)
	     (declare (ignore c))
	     (let ((restart (find-restart 'continue)))
	       (assert-true restart)
	       (invoke-restart restart)))))
    (eval '(defmacro test-locked-package::bar (x)
	    `(list ,x))))
  ;; After the restart, the macro should be defined and expand
  ;; correctly.
  (assert-true (macro-function 'test-locked-package::bar))
  (assert-equal '(list 42)
		(macroexpand-1 '(test-locked-package::bar 42))))

(define-test issue.499.defmacro-unlock-restart
  (:tag :issues)
  (let ((package (find-package :test-locked-package)))
    ;; Set the lock just in case
    (setf (ext:package-definition-lock package) t)
    (handler-bind
	((lisp::package-locked-error
	   #'(lambda (c)
	       (declare (ignore c))
	       (let ((restart (find-restart 'lisp::unlock-package)))
		 (assert-true restart)
		 (invoke-restart restart)))))
      (eval '(defmacro test-locked-package::baz (x)
	      `(list ,x))))
    ;; Macro should be defined, the package lock should be disabled.
    (assert-true (macro-function 'test-locked-package::baz))
    (assert-false (ext:package-definition-lock package))
    (setf (ext::package-definition-lock package) t)))

(define-test issue.499.unlocked-package-no-error
  (:tag :issue)
  (let ((package (find-package :test-locked-package)))
    (setf (ext:package-definition-lock package) nil)
    (unwind-protect
	 (progn
	   (eval '(defmacro test-locked-package::foobar (x)
		   `(list ,x)))
	   (assert-true (macro-function 'test-locked-package::foobar)))
      (setf (ext:package-definition-lock package) t))))
