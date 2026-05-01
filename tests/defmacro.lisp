;;; Tests for defmacro documention and location info

(defpackage :defmacro-tests
  (:use :cl :lisp-unit))

(in-package "DEFMACRO-TESTS")

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Capture the package.
  (defvar *test-package* *package*))

(defmacro issue.497.no-args ()
  `(list))

(defmacro issue.497.simple (a b)
  `(list ,a ,b))

(defmacro issue.497.optional (a &optional (b 0))
  `(* ,a ,b))

(defmacro issue.497.body (name &body body)
  `(progn ,name ,@body))

(defmacro issue.497.with-doc (a b)
  "Build a list from two args"
  `(list ,a ,b))

(define-test issue.497.macro-arglist-no-args
  (:tag :issues)
  (multiple-value-bind (value knownp)
      (c::info :function :macro-arglist 'issue.497.no-args)
    (assert-equal '() value)
    (assert-true knownp)))

(define-test issue.497.macro-arglist-simple
  (:tag :issues)
  (assert-equal '(a b)
		(c::info :function :macro-arglist 'issue.497.simple)))

(define-test issue.497.macro-arglist-optional
  (:tag :issues)
  (assert-equal '(a &optional (b 0))
		(c::info :function :macro-arglist 'issue.497.optional)))

(define-test issue.497.macro-arglist-body
  (:tag :issues)
  (assert-equal '(name &body body)
		(c::info :function :macro-arglist 'issue.497.body)))

(define-test issue.497.documentation
  (:tag :issues)
  (assert-equal "Build a list from two args"
		(documentation 'issue.497.with-doc 'function)))

(define-test issue.497.macro-expands
  (:tag :issues)
  ;; Storing the lambda-list must not have broken the macro-function
  (assert-equal '(list 1 2)
		(macroexpand-1 '(issue.497.simple 1 2))))

(define-test issue.497.describe-runs-cleanly
    (:tag :issues)
  ;; describe must not error on these macros.
  (assert-true
   (stringp (with-output-to-string (*standard-output*)
              (describe 'issue.497.simple))))
  (assert-true
   (stringp (with-output-to-string (*standard-output*)
              (describe 'issue.497.no-args))))
  (assert-true
   (stringp (with-output-to-string (*standard-output*)
              (describe 'issue.497.with-doc))))
  ;; And on a byte-compiled macro from the build itself.
  (assert-true
   (stringp (with-output-to-string (*standard-output*)
              (describe 'ext:letf*)))))

(define-test issue.497.byte-compiled-macro-has-arglist
    (:tag :issues)
  ;; ext:letf* is byte-compiled (extensions.lisp is :byte-compile t)
  ;; and is the canonical example from the bug report.  Its arglist
  ;; should be available now.
  (multiple-value-bind (args winp)
      (c::info :function :macro-arglist 'ext:letf*)
    (assert-true winp)
    (assert-true (consp args))))

(define-test issue.498.source-location
  (:tag :issues)
  (with-input-from-string
    (s (format nil
	       "(in-package ~A)~%(defmacro issue.498.locn (a) `(list ,a))~%"
	       (package-name *test-package*)))
    (ext:compile-from-stream s))
  (assert-true (c::info :source-location :defmacro 'issue.498.locn)))
