;;; xref.lisp -- regression tests for cmucl's xref facility.
;;;
;;; These tests demonstrate that macros used at top level are not
;;; recorded by who-macroexpands.  The bug is in
;;; ir1-convert-global-functoid-no-cmacro (src/compiler/ir1tran.lisp),
;;; which guards the call to (xref:register-xref :macroexpands ...)
;;; with an UNLESS that bails out whenever the lexical environment
;;; has no enclosing BLOCK -- i.e. exactly the situation a top-level
;;; macro use is in.
;;;
;;; Run with the rest of the test suite via run-tests.lisp.
;;;
;;; Note on symbol resolution: the fixture file defines its own
;;; package (XREF-TOPLEVEL-MACRO-FIXTURE) as a top-level DEFPACKAGE,
;;; but we only COMPILE-FILE the fixture -- we don't LOAD the fasl.
;;; DEFPACKAGE at compile-toplevel does cause the package to be made
;;; for the compiler's benefit, so the symbols used as keys in the
;;; xref database are real symbols in a real package after
;;; compile-file returns.  However, we can't write
;;; XREF-TOPLEVEL-MACRO-FIXTURE::TLM-ALPHA in this file because the
;;; reader would try to resolve that package at the time this test
;;; file is itself read -- which is before the fixture has ever been
;;; compiled.  So we resolve symbols at test runtime via FIND-SYMBOL.

(defpackage :xref-tests
  (:use :cl :lisp-unit))

(in-package "XREF-TESTS")

(defparameter *fixture-file*
  #.(merge-pathnames #p"resources/xref-toplevel-macros.lisp"
                     cl:*load-pathname*)
  "Source file containing the macro uses we expect xref to record.")

(defparameter *fixture-package-name* "XREF-TOPLEVEL-MACRO-FIXTURE"
  "Name of the package defined inside the fixture file.")

;; ---------------------------------------------------------------------------
;; Helpers.
;; ---------------------------------------------------------------------------

(defun compile-fixture-with-xref ()
  "Compile *FIXTURE-FILE* with xref recording on.  Clears the xref
database first so the tests are independent of each other and of any
previous compilation.  Returns no values."
  (xref:init-xref-database)
  (let ((c::*record-xref-info* t)
        (*compile-verbose* nil)
        (*compile-print* nil))
    (compile-file *fixture-file*))
  (values))

(defun fixture-symbol (name)
  "Return the symbol named NAME (a string) from the fixture's
package, or NIL if either the package or the symbol doesn't exist."
  (let ((pkg (find-package *fixture-package-name*)))
    (when pkg
      (find-symbol name pkg))))

(defun macro-recorded-p (macro-name)
  "True iff who-macroexpands has at least one entry for the fixture
symbol named MACRO-NAME (a string)."
  (let ((sym (fixture-symbol macro-name)))
    (and sym (not (null (xref:who-macroexpands sym))))))

(defun macro-recorded-from-p (macro-name caller-spec)
  "True iff who-macroexpands has an entry for the fixture symbol
named MACRO-NAME whose context name matches CALLER-SPEC.
CALLER-SPEC is either a string containing a space (treated literally,
e.g. \"Top-Level Form\") or a string naming a symbol in the fixture
package (compared as that symbol)."
  (let ((sym (fixture-symbol macro-name))
        (caller (if (and (stringp caller-spec)
                         (find #\Space caller-spec))
                    caller-spec
                    (fixture-symbol caller-spec))))
    (when sym
      (some (lambda (ctx)
              (equal caller (xref:xref-context-name ctx)))
            (xref:who-macroexpands sym)))))

;; ---------------------------------------------------------------------------
;; Tests.
;; ---------------------------------------------------------------------------

(define-test xref.macro.toplevel.alpha
    (:tag :xref)
  ;; CASE A from the fixture: (tlm-alpha ...) at top level.
  ;; FAILS BEFORE FIX: who-macroexpands returns NIL because the
  ;; UNLESS guard in ir1-convert-global-functoid-no-cmacro suppresses
  ;; the registration when lexenv-blocks is NIL.
  (compile-fixture-with-xref)
  (assert-true (macro-recorded-p "TLM-ALPHA"))
  (assert-true (macro-recorded-from-p "TLM-ALPHA" "Top-Level Form")))

(define-test xref.macro.toplevel.beta
    (:tag :xref)
  ;; CASE A2: (tlm-beta 42) at top level.
  (compile-fixture-with-xref)
  (assert-true (macro-recorded-p "TLM-BETA"))
  (assert-true (macro-recorded-from-p "TLM-BETA" "Top-Level Form")))

(define-test xref.macro.toplevel.gamma
    (:tag :xref)
  ;; CASE A3: (tlm-gamma) at top level, no args.
  (compile-fixture-with-xref)
  (assert-true (macro-recorded-p "TLM-GAMMA"))
  (assert-true (macro-recorded-from-p "TLM-GAMMA" "Top-Level Form")))

(define-test xref.macro.inside-defun
    (:tag :xref)
  ;; CASE B: baseline -- macro used inside a DEFUN.  This already
  ;; works today; the test exists to catch a regression of the
  ;; working path when the bug is fixed.
  (compile-fixture-with-xref)
  (assert-true (macro-recorded-from-p "TLM-ALPHA" "BAR-CALLER")))

(define-test xref.macro.inside-flet
    (:tag :xref)
  ;; CASE B2: baseline -- macro used inside an FLET inside a DEFUN.
  (compile-fixture-with-xref)
  (assert-true (macro-recorded-p "TLM-BETA")))

;; ---------------------------------------------------------------------------
;; Sanity check: with the bug present, the top-level case is missing
;; from the database while the inside-defun case is present.  This
;; test documents that asymmetry directly so a reader of the test
;; output can see at a glance what the bug looks like.
;; ---------------------------------------------------------------------------

(define-test xref.macro.toplevel-vs-defun-symmetry
    (:tag :xref)
  (compile-fixture-with-xref)
  (let ((from-defun    (macro-recorded-from-p "TLM-ALPHA" "BAR-CALLER"))
        (from-toplevel (macro-recorded-from-p "TLM-ALPHA" "Top-Level Form")))
    ;; The DEFUN case is the baseline and must hold both before and
    ;; after the fix.
    (assert-true from-defun)
    ;; The top-level case is what the fix is for.  Before the fix
    ;; this assertion fails; after the fix it should hold.
    (assert-true from-toplevel)))
