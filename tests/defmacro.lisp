;;; Tests for defmacro documention and location info

(defpackage :defmacro-tests
  (:use :cl :lisp-unit))

(in-package "DEFMACRO-TESTS")

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

(define-test issue.497.macro-lambda-list-no-args
  (:tag :issues)
  (multiple-value-bind (value knownp)
      (c::info function lambda-list 'issue.497.no-args)
    (assert-equal '() value)
    (assert-true knownp)))

(define-test issue.497.macro-lambda-list-simple
  (:tag :issues)
  (assert-equal '(a b)
		(c::info function lambda-list 'issue.497.simple)))

(define-test issue.497.macro-lambda-list-optional
  (:tag :issues)
  (assert-equal '(a &optional (b 0))
		(c::info function lambda-list 'issue.497.optional)))

(define-test issue.497.macro-lambda-list-body
  (:tag :issues)
  (assert-equal '(name &body body)
		(c::info function lambda-list 'issue.497.body)))

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
      (c::info function lambda-list 'ext:letf*)
    (assert-true winp)
    (assert-true (consp args))))

;;; Tests for keeping the macro-lambda-list in step with the function kind
;;; when a name stops being (or is re-established as) a macro.

(define-test issue.497.setf-macro-function-clears-arglist
  (:tag :issues)
  ;; A name defined by defmacro has its lambda-list recorded.
  (eval '(defmacro issue.497.setf-mf (a b)
	   `(list ,a ,b)))
  (assert-true
   (nth-value 1 (c::info function lambda-list 'issue.497.setf-mf)))
  ;; Installing a raw expander, whose lambda-list is unknown, must drop
  ;; the recorded arglist.
  (setf (macro-function 'issue.497.setf-mf)
	#'(lambda (whole env)
	    (declare (ignore env))
	    `(list ,@(cdr whole))))
  (multiple-value-bind (args knownp)
      (c::info function lambda-list 'issue.497.setf-mf)
    (assert-false knownp)
    (assert-false args))
  ;; It is still a macro and expands with the newly installed expander.
  (assert-true (macro-function 'issue.497.setf-mf))
  (assert-equal '(list 1 2)
		(macroexpand-1 '(issue.497.setf-mf 1 2)))
  ;; describe must handle a macro with no recorded arglist cleanly.
  (assert-true
   (stringp (with-output-to-string (*standard-output*)
	      (describe 'issue.497.setf-mf)))))

(define-test issue.497.defun-over-macro-clears-arglist
  (:tag :issues)
  (eval '(defmacro issue.497.redef (a b)
	   `(list ,a ,b)))
  (assert-true
   (nth-value 1 (c::info function lambda-list 'issue.497.redef)))
  ;; Redefining the macro as a function clears both the macro
  ;; classification and the recorded macro-lambda-list.
  (handler-bind ((warning #'muffle-warning))
    (eval '(defun issue.497.redef (x) (* x x))))
  (assert-false
   (nth-value 1 (c::info function lambda-list 'issue.497.redef)))
  (assert-false (macro-function 'issue.497.redef))
  (assert-eql 9 (funcall 'issue.497.redef 3)))

(define-test issue.497.fmakunbound-clears-macro
  (:tag :issues)
  (eval '(defmacro issue.497.fmu (a b)
	   `(list ,a ,b)))
  (assert-true (macro-function 'issue.497.fmu))
  (fmakunbound 'issue.497.fmu)
  ;; fmakunbound resets the function kind, so the name is no longer a
  ;; macro even though undefine-function-name leaves the arglist entry.
  (assert-false (macro-function 'issue.497.fmu))
  (assert-false (fboundp 'issue.497.fmu)))

;;; A macro whose expander is compiled along with this file, so describe
;;; exercises the compiled-function path -- the one that regressed when
;;; the info slot was renamed.
(defmacro issue.497.describe-macro (alpha beta &body body)
  "Zorp marker for the issue.497 describe test."
  (declare (ignore alpha beta body))
  nil)

(define-test issue.497.describe-macro-shows-arglist
    (:tag :issues)
  ;; (describe 'macro) must print the macro's lambda-list, not
  ;; "There is no argument information available.".  describe prints
  ;; the lambda-list in lower case, so the argument names appear
  ;; downcased.
  (let ((output (with-output-to-string (*standard-output*)
		  (describe 'issue.497.describe-macro))))
    (assert-true (search "alpha" output))
    (assert-true (search "beta" output)))
  ;; The original bug report: describing the macro-function object
  ;; directly must show the same lambda-list and the doc string.
  (let ((output (with-output-to-string (*standard-output*)
		  (describe (macro-function 'issue.497.describe-macro)))))
    (assert-true (search "alpha" output))
    (assert-true (search "beta" output))
    (assert-true (search "Zorp" output))))

(define-test issue.497.describe-system-macro-shows-arglist
    (:tag :issues)
  ;; A natively-compiled system macro must still show real arguments,
  ;; in the downcased make-arg-names form.
  (let ((output (with-output-to-string (*standard-output*)
		  (describe 'dolist))))
    (assert-true (search "result" output))))

(define-test issue.497.describe-stale-expander
    (:tag :issues)
  ;; Holding an expander past a redefinition must not make describe
  ;; attribute the new lambda-list to the old object.
  (eval '(defmacro issue.497.stale (aaa bbb) `(list ,aaa ,bbb)))
  (let ((old (macro-function 'issue.497.stale)))
    (eval '(defmacro issue.497.stale (xxx yyy zzz)
	     `(list ,xxx ,yyy ,zzz)))
    (let ((output (with-output-to-string (*standard-output*)
		    (describe old))))
      (assert-false (search "xxx" output))
      (assert-false (search "yyy" output)))))

 
