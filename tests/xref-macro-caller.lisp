;;; xref-macro-caller.lisp -- regression test for the caller shape
;;; produced by XREF for references that originate inside a DEFMACRO
;;; expander body.
;;;
;;; Background: xref.lisp's PRETTIEST-CALLER-NAME has a #+nil'd
;;; branch (around line 261) that *looks* like it is responsible for
;;; producing the (:MACRO name) caller form documented in the CMUCL
;;; User's Manual.  Experimentally, that branch is DEAD CODE: the
;;; live branch immediately above it, which tests
;;;
;;;     (and (listp (lambda-name lambda-node))
;;;          (eq :macro (first (lambda-name lambda-node))))
;;;
;;; already produces (:MACRO name) because the compiler assigns the
;;; lambda of a macro expander a lambda-name of exactly that shape.
;;;
;;; This test confirms the live behavior: WHO-CALLS on a function
;;; called from inside a DEFMACRO body returns a context whose name
;;; is the list (:MACRO <macro-symbol>).  Any future change that
;;; regresses this shape -- including an ill-fated attempt to
;;; "fix" the #+nil branch in a way that breaks the live one --
;;; should be caught by this test.

(defpackage :xref-macro-caller-tests
  (:use :cl :lisp-unit))

(in-package "XREF-MACRO-CALLER-TESTS")

(defparameter *fixture-file*
  #.(merge-pathnames #p"resources/xref-macro-caller-fixture.lisp"
                     cl:*load-pathname*)
  "Source file whose compilation populates the xref database.")

(defparameter *fixture-package-name* "XREF-MACRO-CALLER-FIXTURE")

(defun compile-fixture-with-xref ()
  (xref:init-xref-database)
  (let ((c::*record-xref-info* t)
        (*compile-verbose* nil)
        (*compile-print* nil))
    (compile-file *fixture-file*))
  (values))

(defun fixture-symbol (name)
  (let ((pkg (find-package *fixture-package-name*)))
    (when pkg
      (find-symbol name pkg))))

(defun caller-names-for-call (callee-name-string)
  "Return the list of caller-name values recorded for WHO-CALLS on
the fixture symbol named CALLEE-NAME-STRING."
  (let ((sym (fixture-symbol callee-name-string)))
    (when sym
      (mapcar #'xref:xref-context-name (xref:who-calls sym)))))

(defun caller-matches-macro-form-p (caller-name macro-name-string)
  "True iff CALLER-NAME is (:MACRO <sym>) where <sym> is the fixture
symbol named MACRO-NAME-STRING."
  (let ((sym (fixture-symbol macro-name-string)))
    (and sym
         (consp caller-name)
         (eq :macro (first caller-name))
         (eq sym (second caller-name)))))

;; ---------------------------------------------------------------------------
;; Diagnostic test -- always passes, just prints what we observed.
;; Useful when a future change breaks the real test below; this one
;; will show you the new shape of the caller name.
;; ---------------------------------------------------------------------------

(define-test xref.macro-caller.dump
    (:tag :xref-macro-caller)
  (compile-fixture-with-xref)
  (format t "~&~%=== caller names recorded for HELPER ===~%")
  (format t "  WHO-CALLS helper -> ~S~%"
          (caller-names-for-call "HELPER"))
  (format t "=== end ===~%")
  (assert-true t))

;; ---------------------------------------------------------------------------
;; Real test: a call from inside a DEFMACRO body is reported with a
;; caller name of the form (:MACRO macro-name).
;; ---------------------------------------------------------------------------

(define-test xref.macro-caller.shape
    (:tag :xref-macro-caller)
  (compile-fixture-with-xref)
  (let ((names (caller-names-for-call "HELPER")))
    (assert-true
     (some (lambda (n) (caller-matches-macro-form-p n "MAC-A"))
           names)
     names)))
