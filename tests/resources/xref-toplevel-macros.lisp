;;; xref-toplevel-macros.lisp -- fixture for xref top-level macro tests.
;;;
;;; This file is COMPILED by the test harness (with c::*record-xref-info*
;;; bound to T).  It is not loaded for execution; the test only inspects
;;; the xref database afterwards.  Therefore none of the macros below
;;; need to do anything sensible at runtime -- they only need to be
;;; macros, so that the compiler takes the :macro branch in
;;; ir1-convert-global-functoid-no-cmacro.

(defpackage :xref-toplevel-macro-fixture
  (:use :cl))

(in-package :xref-toplevel-macro-fixture)

;; ---------------------------------------------------------------------------
;; Macros under test.  Each one expands to something innocuous; what we care
;; about is that the use-site is recorded in xref::*who-macroexpands*.
;; ---------------------------------------------------------------------------

(defmacro tlm-alpha (&body body)
  `(progn ,@body))

(defmacro tlm-beta (x)
  `(identity ,x))

(defmacro tlm-gamma ()
  `(values))

;; ---------------------------------------------------------------------------
;; CASE A: macro used directly at top level.
;;
;; This is the case the bug is about.  The form below is a top-level
;; call to TLM-ALPHA.  At the point ir1-convert sees it, the lexical
;; environment has no enclosing BLOCK, so (lexenv-blocks ...) is NIL
;; and the buggy guard skips the call to register-xref :macroexpands.
;;
;; After the fix, who-macroexpands should report TLM-ALPHA expanded
;; from a context whose name is "Top-Level Form".
;; ---------------------------------------------------------------------------

(tlm-alpha
  (defparameter *tlm-alpha-marker* t))

;; CASE A2: another top-level use, of a different macro, just to make
;; sure the fix isn't accidentally specific to one macro.

(tlm-beta 42)

;; CASE A3: top-level use with no arguments.

(tlm-gamma)

;; ---------------------------------------------------------------------------
;; CASE B: macro used inside a DEFUN.
;;
;; This is the BASELINE case -- it already works today, because DEFUN
;; establishes an implicit BLOCK with the function's name, so
;; (caar (lexenv-blocks ...)) yields BAR-CALLER and the existing code
;; records it.  We test it so that the regression test will catch a
;; future regression of the working path as well.
;; ---------------------------------------------------------------------------

(defun bar-caller ()
  (tlm-alpha 1 2 3))

;; CASE B2: macro used inside a nested function.

(defun baz-caller ()
  (flet ((inner () (tlm-beta 7)))
    (inner)))
